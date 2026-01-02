use bevy::prelude::*;
use bevy::window::PrimaryWindow;
use bevy_svg::prelude::*;
use std::collections::HashMap;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct BoardPos {
    file: i32,
    rank: i32,
}

impl BoardPos {
    fn new(file: i32, rank: i32) -> Option<Self> {
        if file >= 0 && file < 8 && rank >= 0 && rank < 8 {
            Some(Self { file, rank })
        } else {
            None
        }
    }

    fn offset(&self, file_offset: i32, rank_offset: i32) -> Option<Self> {
        Self::new(self.file + file_offset, self.rank + rank_offset)
    }

    fn from_world(translation: Vec2, square_size: f32) -> Option<Self> {
        let file = 7 - ((translation.x / square_size) + 4.0).floor() as i32;
        let rank = 7 - ((translation.y / square_size) + 4.0).floor() as i32;
        Self::new(file, rank)
    }

    fn to_world(&self, square_size: f32, z: f32) -> Vec3 {
        Vec3::new(
            square_size * (((7 - self.file) as f32 + 0.5) - 4.0),
            square_size * (((7 - self.rank) as f32 + 0.5) - 4.0),
            z,
        )
    }
}

#[derive(Component)]
struct MoveHighlight;

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
enum PieceKind {
    King,
    Queen,
    Rook,
    Bishop,
    Knight,
    Pawn,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
enum PieceColor {
    White,
    Black,
}

impl PieceColor {
    fn opposite(&self) -> Self {
        match self {
            PieceColor::White => PieceColor::Black,
            PieceColor::Black => PieceColor::White,
        }
    }
}

#[derive(Component)]
struct ChessPiece {
    kind: PieceKind,
    color: PieceColor,
}

#[derive(Resource, Default)]
struct MovementState {
    selected_entity: Option<Entity>,
    start_position: Option<BoardPos>,
    valid_moves: Vec<BoardPos>,
    is_dragging: bool,
}

#[derive(Resource, Clone)]
struct Board {
    squares: [[Option<(PieceColor, PieceKind)>; 8]; 8],
    active_color: PieceColor,
}

impl Board {
    const SQUARE_SIZE: f32 = 60.0;

    fn get_piece(&self, pos: BoardPos) -> Option<(PieceColor, PieceKind)> {
        self.squares[pos.rank as usize][pos.file as usize]
    }

    fn move_piece(&mut self, from: BoardPos, to: BoardPos) -> bool {
        if let Some((moving_color, _)) = self.squares[from.rank as usize][from.file as usize] {
            if moving_color != self.active_color {
                return false;
            }

            let piece = self.squares[from.rank as usize][from.file as usize].take();
            self.squares[to.rank as usize][to.file as usize] = piece;
            self.active_color = self.active_color.opposite();
            true
        } else {
            false
        }
    }

    fn from_fen(fen: &str) -> Result<Self, String> {
        let mut squares = [[None; 8]; 8];
        let parts: Vec<&str> = fen.split_ascii_whitespace().collect();
        let ranks: Vec<&str> = parts[0].split('/').collect();

        for (rank_idx, rank) in ranks.iter().enumerate() {
            let mut file_idx = 0;
            for c in rank.chars() {
                if c.is_digit(10) {
                    file_idx += c.to_digit(10).unwrap() as usize;
                } else {
                    let color = if c.is_uppercase() {
                        PieceColor::White
                    } else {
                        PieceColor::Black
                    };
                    let kind = match c.to_ascii_lowercase() {
                        'k' => PieceKind::King,
                        'q' => PieceKind::Queen,
                        'r' => PieceKind::Rook,
                        'b' => PieceKind::Bishop,
                        'n' => PieceKind::Knight,
                        'p' => PieceKind::Pawn,
                        _ => return Err(format!("Invalid piece: {}", c)),
                    };
                    squares[rank_idx][7 - file_idx] = Some((color, kind));
                    file_idx += 1;
                }
            }
        }

        Ok(Board {
            squares,
            active_color: PieceColor::White,
        })
    }
}

fn handle_piece_movement(
    mut commands: Commands,
    mut pieces: Query<(Entity, &mut Transform, &ChessPiece)>,
    mouse: Res<ButtonInput<MouseButton>>,
    camera: Query<(&Camera, &GlobalTransform)>,
    windows: Query<&Window, With<PrimaryWindow>>,
    mut movement_state: ResMut<MovementState>,
    mut board: ResMut<Board>,
) {
    let (camera, camera_transform) = camera.single();
    let window = windows.single();

    let cursor_world_pos = window
        .cursor_position()
        .and_then(|cursor| camera.viewport_to_world_2d(camera_transform, cursor).ok());

    let cursor_board_pos =
        cursor_world_pos.and_then(|world_pos| BoardPos::from_world(world_pos, Board::SQUARE_SIZE));

    if mouse.just_pressed(MouseButton::Left) {
        if let Some(pos) = cursor_board_pos {
            // If we have a piece selected and click a valid move position
            if let Some(_) = movement_state.selected_entity {
                if movement_state.valid_moves.contains(&pos) {
                    finalize_movement(
                        &mut commands,
                        &mut pieces,
                        pos,
                        &mut board,
                        &mut movement_state,
                    );
                    return;
                }
            }

            // Try to select a piece and immediately start "dragging"
            handle_piece_selection(&mut pieces, pos, &board, &mut movement_state);
            if movement_state.selected_entity.is_some() {
                movement_state.is_dragging = true;
                // Center the piece initially
                if let Some(entity) = movement_state.selected_entity {
                    if let Ok((_, mut transform, _)) = pieces.get_mut(entity) {
                        transform.translation = pos.to_world(Board::SQUARE_SIZE, 0.1);
                    }
                }
            }
        }
    }

    if mouse.pressed(MouseButton::Left) && movement_state.is_dragging {
        if let Some(cursor_world_pos) = cursor_world_pos {
            update_dragged_piece(&mut pieces, cursor_world_pos, &movement_state);
        }
    }

    if mouse.just_released(MouseButton::Left) {
        if let Some(target_pos) = cursor_board_pos {
            if let Some(start_pos) = movement_state.start_position {
                if target_pos == start_pos {
                    // Released in same square - treat as click/selection
                    if let Some(entity) = movement_state.selected_entity {
                        movement_state.is_dragging = false;
                        // Center the piece in its square
                        if let Ok((_, mut transform, _)) = pieces.get_mut(entity) {
                            transform.translation = start_pos.to_world(Board::SQUARE_SIZE, 0.1);
                        }
                        return;
                    }
                }
            }
            finalize_movement(
                &mut commands,
                &mut pieces,
                target_pos,
                &mut board,
                &mut movement_state,
            );
        } else {
            // Reset piece position if released outside board
            if let Some(entity) = movement_state.selected_entity {
                if let Some(start_pos) = movement_state.start_position {
                    if let Ok((_, mut transform, _)) = pieces.get_mut(entity) {
                        transform.translation = start_pos.to_world(Board::SQUARE_SIZE, 0.1);
                    }
                }
            }
            // Clear everything since we released outside the board
            movement_state.selected_entity = None;
            movement_state.start_position = None;
            movement_state.valid_moves.clear();
            movement_state.is_dragging = false;
        }
    }
}

fn handle_piece_selection(
    pieces: &mut Query<(Entity, &mut Transform, &ChessPiece)>,
    pos: BoardPos,
    board: &Board,
    movement_state: &mut MovementState,
) {
    // If clicking the same piece, deselect it
    if let Some(current_pos) = movement_state.start_position {
        if current_pos == pos {
            movement_state.selected_entity = None;
            movement_state.start_position = None;
            movement_state.valid_moves.clear();
            movement_state.is_dragging = false;
            return;
        }
    }

    // Try to select a piece
    for (entity, transform, piece) in pieces.iter() {
        if let Some(piece_pos) =
            BoardPos::from_world(transform.translation.truncate(), Board::SQUARE_SIZE)
        {
            if piece_pos == pos && piece.color == board.active_color {
                movement_state.selected_entity = Some(entity);
                movement_state.start_position = Some(pos);
                movement_state.valid_moves = calculate_valid_moves(pos, piece, board);
                break;
            }
        }
    }
}

fn update_dragged_piece(
    pieces: &mut Query<(Entity, &mut Transform, &ChessPiece)>,
    cursor_world_pos: Vec2,
    movement_state: &MovementState,
) {
    if let Some(entity) = movement_state.selected_entity {
        if let Ok((_, mut transform, _)) = pieces.get_mut(entity) {
            if movement_state.is_dragging {
                transform.translation.x = cursor_world_pos.x;
                transform.translation.y = cursor_world_pos.y;
                transform.translation.z = 0.2;
            }
        }
    }
}

fn finalize_movement(
    commands: &mut Commands,
    pieces: &mut Query<(Entity, &mut Transform, &ChessPiece)>,
    target_pos: BoardPos,
    board: &mut Board,
    movement_state: &mut MovementState,
) {
    if let Some(entity) = movement_state.selected_entity {
        if let Some(start_pos) = movement_state.start_position {
            if movement_state.valid_moves.contains(&target_pos) {
                // Handle capture
                for (other_entity, transform, _) in pieces.iter() {
                    if other_entity != entity {
                        if let Some(other_pos) = BoardPos::from_world(
                            transform.translation.truncate(),
                            Board::SQUARE_SIZE,
                        ) {
                            if other_pos == target_pos {
                                commands.entity(other_entity).despawn();
                                break;
                            }
                        }
                    }
                }

                // Move piece
                if let Ok((_, mut transform, _)) = pieces.get_mut(entity) {
                    transform.translation = target_pos.to_world(Board::SQUARE_SIZE, 0.1);
                    board.move_piece(start_pos, target_pos);
                }
            } else {
                // Invalid move - snap back
                if let Ok((_, mut transform, _)) = pieces.get_mut(entity) {
                    transform.translation = start_pos.to_world(Board::SQUARE_SIZE, 0.1);
                }
            }
        }
    }

    // Reset movement state
    movement_state.selected_entity = None;
    movement_state.start_position = None;
    movement_state.valid_moves.clear();
    movement_state.is_dragging = false;
}

fn handle_highlights(
    mut commands: Commands,
    movement_state: Res<MovementState>,
    highlights: Query<Entity, With<MoveHighlight>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
    mut meshes: ResMut<Assets<Mesh>>,
) {
    for entity in highlights.iter() {
        commands.entity(entity).despawn();
    }

    if !movement_state.valid_moves.is_empty() {
        let material = materials.add(ColorMaterial::from(Color::srgba(0.0, 1.0, 0.0, 0.3)));
        let mesh = meshes.add(Circle::new(25.0));

        for &pos in &movement_state.valid_moves {
            commands.spawn((
                MoveHighlight,
                Mesh2d(mesh.clone()),
                MeshMaterial2d(material.clone()),
                Transform::from_translation(pos.to_world(Board::SQUARE_SIZE, 0.05)),
            ));
        }
    }
}

fn calculate_sliding_moves(
    pos: BoardPos,
    color: PieceColor,
    directions: &[(i32, i32)],
    board: &Board,
) -> Vec<BoardPos> {
    let mut moves = Vec::new();

    for &(file_dir, rank_dir) in directions {
        // For each direction, keep moving until we hit a piece or the board edge
        for step in 1..8 {
            if let Some(new_pos) = pos.offset(file_dir * step, rank_dir * step) {
                match board.get_piece(new_pos) {
                    None => {
                        // Empty square, we can move here
                        moves.push(new_pos);
                    }
                    Some((piece_color, _)) => {
                        // Hit a piece
                        if piece_color != color {
                            // Can capture enemy piece
                            moves.push(new_pos);
                        }
                        // Either way, can't move further in this direction
                        break;
                    }
                }
            } else {
                // Hit board edge
                break;
            }
        }
    }

    moves
}

fn calculate_valid_moves(pos: BoardPos, piece: &ChessPiece, board: &Board) -> Vec<BoardPos> {
    match piece.kind {
        PieceKind::Pawn => calculate_pawn_moves(pos, piece.color, board),
        PieceKind::Knight => calculate_knight_moves(pos, piece.color, board),
        PieceKind::Bishop => calculate_bishop_moves(pos, piece.color, board),
        PieceKind::Rook => calculate_rook_moves(pos, piece.color, board),
        PieceKind::Queen => calculate_queen_moves(pos, piece.color, board),
        PieceKind::King => calculate_king_moves(pos, piece.color, board),
    }
}

fn calculate_pawn_moves(pos: BoardPos, color: PieceColor, board: &Board) -> Vec<BoardPos> {
    let direction = if color == PieceColor::White { -1 } else { 1 };
    let starting_rank = if color == PieceColor::White { 6 } else { 1 };
    let mut moves = Vec::new();

    // Forward moves
    if let Some(forward) = pos.offset(0, direction) {
        if board.get_piece(forward).is_none() {
            moves.push(forward);

            if pos.rank == starting_rank {
                if let Some(double) = pos.offset(0, 2 * direction) {
                    if board.get_piece(double).is_none() {
                        moves.push(double);
                    }
                }
            }
        }
    }

    // Captures
    for capture_offset in [-1, 1] {
        if let Some(capture_pos) = pos.offset(capture_offset, direction) {
            if let Some((piece_color, _)) = board.get_piece(capture_pos) {
                if piece_color != color {
                    moves.push(capture_pos);
                }
            }
        }
    }

    moves
}

fn calculate_knight_moves(pos: BoardPos, color: PieceColor, board: &Board) -> Vec<BoardPos> {
    let offsets = [
        (1, 2),
        (2, 1),
        (2, -1),
        (1, -2),
        (-1, -2),
        (-2, -1),
        (-2, 1),
        (-1, 2),
    ];

    offsets
        .iter()
        .filter_map(|(df, dr)| pos.offset(*df, *dr))
        .filter(|&pos| match board.get_piece(pos) {
            None => true,
            Some((piece_color, _)) => piece_color != color,
        })
        .collect()
}

// Now we can simplify the bishop moves
fn calculate_bishop_moves(pos: BoardPos, color: PieceColor, board: &Board) -> Vec<BoardPos> {
    let diagonal_directions = [(1, 1), (1, -1), (-1, 1), (-1, -1)];
    calculate_sliding_moves(pos, color, &diagonal_directions, board)
}

// And the rook moves
fn calculate_rook_moves(pos: BoardPos, color: PieceColor, board: &Board) -> Vec<BoardPos> {
    let orthogonal_directions = [(0, 1), (1, 0), (-1, 0), (0, -1)];
    calculate_sliding_moves(pos, color, &orthogonal_directions, board)
}

// And implement queen moves by combining both direction sets
fn calculate_queen_moves(pos: BoardPos, color: PieceColor, board: &Board) -> Vec<BoardPos> {
    let all_directions = [
        (1, 1),
        (1, -1),
        (-1, 1),
        (-1, -1),
        (0, 1),
        (1, 0),
        (-1, 0),
        (0, -1),
    ];
    calculate_sliding_moves(pos, color, &all_directions, board)
}

fn calculate_king_moves(pos: BoardPos, color: PieceColor, board: &Board) -> Vec<BoardPos> {
    let offsets = [
        (1, 1),
        (1, -1),
        (-1, 1),
        (-1, -1),
        (0, 1),
        (1, 0),
        (-1, 0),
        (0, -1),
    ];

    offsets
        .iter()
        .filter_map(|(df, dr)| pos.offset(*df, *dr))
        .filter(|&pos| match board.get_piece(pos) {
            None => true,
            Some((piece_color, _)) => piece_color != color,
        })
        .collect()
}

#[derive(Resource, Clone)]
struct PieceAssets {
    handles: HashMap<(PieceColor, PieceKind), Handle<Svg>>,
}

impl PieceAssets {
    fn load_all(asset_server: &AssetServer) -> Self {
        let mut handles = HashMap::new();

        // White pieces
        handles.insert(
            (PieceColor::White, PieceKind::Pawn),
            asset_server.load("pieces/Chess_plt45.svg"),
        );
        handles.insert(
            (PieceColor::White, PieceKind::Knight),
            asset_server.load("pieces/Chess_nlt45.svg"),
        );
        handles.insert(
            (PieceColor::White, PieceKind::Bishop),
            asset_server.load("pieces/Chess_blt45.svg"),
        );
        handles.insert(
            (PieceColor::White, PieceKind::Rook),
            asset_server.load("pieces/Chess_rlt45.svg"),
        );
        handles.insert(
            (PieceColor::White, PieceKind::Queen),
            asset_server.load("pieces/Chess_qlt45.svg"),
        );
        handles.insert(
            (PieceColor::White, PieceKind::King),
            asset_server.load("pieces/Chess_klt45.svg"),
        );

        // Black pieces
        handles.insert(
            (PieceColor::Black, PieceKind::Pawn),
            asset_server.load("pieces/Chess_pdt45.svg"),
        );
        handles.insert(
            (PieceColor::Black, PieceKind::Knight),
            asset_server.load("pieces/Chess_ndt45.svg"),
        );
        handles.insert(
            (PieceColor::Black, PieceKind::Bishop),
            asset_server.load("pieces/Chess_bdt45.svg"),
        );
        handles.insert(
            (PieceColor::Black, PieceKind::Rook),
            asset_server.load("pieces/Chess_rdt45.svg"),
        );
        handles.insert(
            (PieceColor::Black, PieceKind::Queen),
            asset_server.load("pieces/Chess_qdt45.svg"),
        );
        handles.insert(
            (PieceColor::Black, PieceKind::King),
            asset_server.load("pieces/Chess_kdt45.svg"),
        );

        PieceAssets { handles }
    }
}

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
    asset_server: Res<AssetServer>,
) {
    commands.spawn(Camera2d);

    let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
    let board = Board::from_fen(fen).expect("Invalid FEN string");
    let piece_assets = PieceAssets::load_all(&asset_server);

    commands.insert_resource(piece_assets.clone());
    commands.insert_resource(board.clone());

    // Spawn board squares
    let square = meshes.add(Rectangle::new(Board::SQUARE_SIZE, Board::SQUARE_SIZE));

    for rank in 0..8 {
        for file in 0..8 {
            let pos = BoardPos::new(file, rank).unwrap();
            commands.spawn((
                Mesh2d(square.clone()),
                MeshMaterial2d(materials.add(if (rank + file) % 2 == 0 {
                    Color::srgb(0.25, 0.25, 0.25)
                } else {
                    Color::srgb(0.75, 0.75, 0.75)
                })),
                Transform::from_translation(pos.to_world(Board::SQUARE_SIZE, 0.0)),
            ));

            // Spawn pieces
            if let Some((color, kind)) = board.get_piece(pos) {
                if let Some(handle) = piece_assets.handles.get(&(color, kind)) {
                    commands.spawn((
                        Svg2d(handle.clone()),
                        Origin::Center,
                        Transform::from_translation(pos.to_world(Board::SQUARE_SIZE, 0.1)),
                        ChessPiece { color, kind },
                    ));
                }
            }
        }
    }
}

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugins(bevy_svg::prelude::SvgPlugin)
        .insert_resource(MovementState::default())
        .add_systems(Startup, setup)
        .add_systems(Update, (handle_piece_movement, handle_highlights).chain())
        .run();
}
