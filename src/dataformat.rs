use std::path::Path;

use crate::chess::{
    board::{Board, GameOutcome},
    chessmove::Move,
};

use self::marlinformat::{util::I16Le, PackedBoard};
use anyhow::Context;
use rand::{rngs::ThreadRng, Rng};
use serde::{Deserialize, Serialize};

mod marlinformat;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[allow(clippy::upper_case_acronyms)]
pub enum WDL {
    Win,
    Loss,
    Draw,
}

fn serialise_33_f64_array<S>(arr: &[f64; 33], serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    arr.serialize(serializer)
}

fn deserialise_33_f64_array<'de, D>(deserializer: D) -> Result<[f64; 33], D::Error>
where
    D: serde::Deserializer<'de>,
{
    let vec: Vec<f64> = Vec::deserialize(deserializer)?;
    if vec.len() != 33 {
        return Err(serde::de::Error::custom(format!(
            "expected array of length 33, got length {}",
            vec.len()
        )));
    }
    let mut arr = [0.0; 33];
    arr.copy_from_slice(&vec);
    Ok(arr)
}

/// The configuration for a filter that can be applied to a game during unpacking.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[allow(clippy::struct_field_names)]
#[serde(default)]
pub struct Filter {
    /// Filter out positions that have a ply count less than this value.
    pub min_ply: u32,
    /// Filter out positions that have fewer pieces on the board than this value.
    pub min_pieces: u32,
    /// Filter out positions that have an absolute evaluation above this value.
    pub max_eval: u32,
    /// Filter out positions where a tactical move was made.
    pub filter_tactical: bool,
    /// Filter out positions that are in check.
    pub filter_check: bool,
    /// Filter out positions where a castling move was made.
    pub filter_castling: bool,
    /// Filter out positions where eval diverges from WDL by more than this value.
    pub max_eval_incorrectness: u32,
    /// Whether to randomly skip positions.
    pub random_fen_skipping: bool,
    /// The probability of skipping a position when `random_fen_skipping` is enabled.
    pub random_fen_skip_probability: f64,
    /// Whether to skip positions based on the WDL model.
    pub wdl_filtered: bool,
    /// The first set of parameters for the WDL model.
    pub wdl_model_params_a: [f64; 4],
    /// The second set of parameters for the WDL model.
    pub wdl_model_params_b: [f64; 4],
    /// The lower material count limit for the WDL model.
    pub material_min: u32,
    /// The upper material count limit for the WDL model.
    pub material_max: u32,
    /// The polynomials p_a and p_b will be expressed in terms for the WDL model.
    pub mom_target: u32,
    /// The internal heuristic scale factor for the WDL model.
    pub wdl_heuristic_scale: f64,
    /// Whether to skip positions based on the material count table.
    pub material_count_filtered: bool,
    /// Probability that a position with a particular material count is skipped.
    #[serde(serialize_with = "serialise_33_f64_array")]
    #[serde(deserialize_with = "deserialise_33_f64_array")]
    pub material_count_probabilities: [f64; 33],
}

impl Default for Filter {
    fn default() -> Self {
        Self {
            min_ply: 16,
            min_pieces: 4,
            max_eval: 31339,
            filter_tactical: true,
            filter_check: true,
            filter_castling: false,
            max_eval_incorrectness: u32::MAX,
            random_fen_skipping: false,
            random_fen_skip_probability: 0.0,
            wdl_filtered: false,
            wdl_model_params_a: [6.871_558_62, -39.652_263_91, 90.684_603_52, 170.669_963_64],
            wdl_model_params_b: [
                -7.198_907_10,
                56.139_471_85,
                -139.910_911_83,
                182.810_074_27,
            ],
            material_min: 17,
            material_max: 78,
            mom_target: 58,
            wdl_heuristic_scale: 1.5,
            material_count_filtered: false,
            material_count_probabilities: [0.0; 33],
        }
    }
}

impl Filter {
    pub const UNRESTRICTED: Self = Self {
        min_ply: 0,
        min_pieces: 0,
        max_eval: u32::MAX,
        filter_tactical: false,
        filter_check: false,
        filter_castling: false,
        max_eval_incorrectness: u32::MAX,
        random_fen_skipping: false,
        random_fen_skip_probability: 0.0,
        wdl_filtered: false,
        wdl_model_params_a: [0.0; 4],
        wdl_model_params_b: [0.0; 4],
        material_min: 17,
        material_max: 78,
        mom_target: 58,
        wdl_heuristic_scale: 1.0,
        material_count_filtered: false,
        material_count_probabilities: [0.0; 33],
    };

    fn wdl_model(&self, material: u32, eval: i32) -> (f64, f64, f64) {
        let m = (material.clamp(self.material_min, self.material_max) as f64)
            / (self.mom_target as f64);

        let p_as = &self.wdl_model_params_a;
        let p_bs = &self.wdl_model_params_b;

        let a = p_as[0]
            .mul_add(m, p_as[1])
            .mul_add(m, p_as[2])
            .mul_add(m, p_as[3]);
        let b = p_bs[0]
            .mul_add(m, p_bs[1])
            .mul_add(m, p_bs[2])
            .mul_add(m, p_bs[3]);

        let b = b * self.wdl_heuristic_scale;

        let x = eval as f64;
        let w = 1.0 / (1.0 + f64::exp((a - x) / b));
        let l = 1.0 / (1.0 + f64::exp((a + x) / b));
        let d = (1.0 - w - l).max(0.0); // avoid negative draw due to float precision.

        (w, d, l)
    }

    fn result_chance(&self, material: u32, eval: i32, wdl: WDL) -> f64 {
        let (win, draw, loss) = self.wdl_model(material, eval);
        match wdl {
            WDL::Win => win,
            WDL::Draw => draw,
            WDL::Loss => loss,
        }
    }

    pub fn should_filter(
        &self,
        mv: Move,
        eval: i32,
        board: &Board,
        wdl: WDL,
        rng: &mut ThreadRng,
    ) -> bool {
        if board.ply() < self.min_ply as usize {
            return true;
        }
        if eval.unsigned_abs() >= self.max_eval {
            return true;
        }
        if board.pieces.occupied().count() < self.min_pieces {
            return true;
        }
        if self.filter_tactical && board.is_tactical(mv) {
            return true;
        }
        if self.filter_check && board.in_check() {
            return true;
        }
        if self.filter_castling && mv.is_castle() {
            return true;
        }
        if self.random_fen_skipping && rng.random_bool(self.random_fen_skip_probability) {
            return true;
        }
        if self.wdl_filtered
            && rng.random_bool(1.0 - self.result_chance(board.material_count(), eval, wdl))
        {
            return true;
        }
        if self.material_count_filtered {
            let index = board.pieces.occupied().count().min(32) as usize;
            let prob = self.material_count_probabilities[index];
            if rng.random_bool(prob) {
                return true;
            }
        }
        if self.max_eval_incorrectness != u32::MAX {
            // if the game was a draw, prune evals that are too far away from a draw.
            if wdl == WDL::Draw && eval.unsigned_abs() > self.max_eval_incorrectness {
                return true;
            }
            // otherwise, if the winner's eval drops too low, prune.
            let winner_pov_eval = if wdl == WDL::Win {
                // if white won, get white's eval.
                eval
            } else {
                // if black won, get black's eval.
                -eval
            };
            // clamp winner_pov_eval down to 0, check size.
            if winner_pov_eval.min(0).unsigned_abs() > self.max_eval_incorrectness {
                // too high for the losing side.
                return true;
            }
        }
        false
    }

    pub fn from_path(path: &Path) -> Result<Self, anyhow::Error> {
        let text = std::fs::read_to_string(path)
            .with_context(|| format!("Failed to read filter config file at {path:?}"))?;
        toml::from_str(&text).with_context(|| {
            let default = toml::to_string_pretty(&Self::default()).unwrap();
            format!("Failed to parse filter config file at {path:?} \nNote: the config file must be in TOML format. The default config looks like this: \n```\n{default}```")
        })
    }
}

/// A game annotated with evaluations starting from a potentially custom position, with support for efficent binary serialisation and deserialisation.
#[derive(PartialEq, Eq, Debug)]
pub struct Game {
    /// The initial position of the self-play game.
    pub initial_position: marlinformat::PackedBoard,
    /// The moves played in the self-play game, along with the evaluation of the position in which they were played.
    pub moves: Vec<(Move, marlinformat::util::I16Le)>,
}

const SEQUENCE_ELEM_SIZE: usize =
    std::mem::size_of::<Move>() + std::mem::size_of::<marlinformat::util::I16Le>();
const NULL_TERMINATOR: [u8; SEQUENCE_ELEM_SIZE] = [0; SEQUENCE_ELEM_SIZE];

impl WDL {
    pub fn from_packed(packed: u8) -> Self {
        match packed {
            2 => Self::Win,
            1 => Self::Draw,
            0 => Self::Loss,
            _ => panic!("invalid WDL, expected 0, 1, or 2, got {packed}"),
        }
    }
}

impl Game {
    pub const MAX_SPLATTABLE_GAME_SIZE: usize = 512;

    pub fn new(initial_position: &Board) -> Self {
        Self {
            initial_position: initial_position.to_marlinformat(0, 0, 0),
            moves: Vec::new(),
        }
    }

    pub fn initial_position(&self) -> Board {
        self.initial_position.unpack().0
    }

    pub fn moves(&self) -> impl Iterator<Item = Move> + '_ {
        self.moves.iter().map(|(mv, _)| *mv)
    }

    pub fn set_outcome(&mut self, outcome: GameOutcome) {
        self.initial_position.set_outcome(outcome);
    }

    pub fn outcome(&self) -> WDL {
        let (_, _, wdl, _) = self.initial_position.unpack();
        WDL::from_packed(wdl)
    }

    pub fn add_move(&mut self, mv: Move, eval: i16) {
        self.moves.push((mv, marlinformat::util::I16Le::new(eval)));
    }

    pub fn len(&self) -> usize {
        self.moves.len()
    }

    pub fn is_empty(&self) -> bool {
        self.moves.is_empty()
    }

    /// Serialises the game into a byte stream.
    pub fn serialise_into(&self, writer: &mut impl std::io::Write) -> std::io::Result<()> {
        writer.write_all(&self.initial_position.as_bytes())?;
        for (mv, eval) in &self.moves {
            writer.write_all(&mv.inner().to_le_bytes())?;
            writer.write_all(&eval.get().to_le_bytes())?;
        }
        writer.write_all(&NULL_TERMINATOR)?;
        Ok(())
    }

    /// Deserialises a game from a byte stream.
    pub fn deserialise_from(
        reader: &mut impl std::io::BufRead,
        buffer: Vec<(Move, marlinformat::util::I16Le)>,
    ) -> std::io::Result<Self> {
        let mut initial_position = [0; std::mem::size_of::<marlinformat::PackedBoard>()];
        reader.read_exact(&mut initial_position)?;
        let initial_position = PackedBoard::from_bytes(initial_position);
        #[cfg(debug_assertions)]
        let (mut real_board, _, _, _) = initial_position.unpack();
        #[cfg(debug_assertions)]
        if let Err(problem) = real_board.check_validity() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!("marlinformat header malformed: {problem}"),
            ));
        }
        // we allow the caller to give us a pre-allocated buffer as an optimisation
        let mut moves = buffer;
        moves.clear();
        loop {
            let mut buf = [0; SEQUENCE_ELEM_SIZE];
            reader.read_exact(&mut buf)?;
            if buf == NULL_TERMINATOR {
                break;
            }
            let mv = Move::from_raw(u16::from_le_bytes([buf[0], buf[1]]));
            let Some(mv) = mv else {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "parsed invalid move - move was null (the all-zeroes bitpattern)".to_string(),
                ));
            };
            if !mv.is_valid() || mv.from() == mv.to() {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!("parsed invalid move: {mv:?}"),
                ));
            }
            #[cfg(debug_assertions)]
            if !real_board.legal_moves().contains(&mv) {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!("parsed illegal move: {mv:?}"),
                ));
            }
            let eval = I16Le::new(i16::from_le_bytes([buf[2], buf[3]]));
            moves.push((mv, eval));
            #[cfg(debug_assertions)]
            real_board.make_move_simple(mv);
        }
        Ok(Self {
            initial_position,
            moves,
        })
    }

    /// Just deserialises the bytes of one game into a buffer, without checking the validity of the moves.
    /// This is used for fast deserialisation of games that are already known to be valid.
    ///
    /// WARNING: This function does not clear the buffer, so it will append to the existing contents.
    /// It is the caller's responsibility to ensure that the buffer is cleared when necessary.
    pub fn deserialise_fast_into_buffer(
        reader: &mut impl std::io::BufRead,
        buffer: &mut Vec<u8>,
    ) -> std::io::Result<()> {
        let mut initial_position = [0; std::mem::size_of::<marlinformat::PackedBoard>()];
        reader.read_exact(&mut initial_position)?;
        buffer.extend_from_slice(&initial_position);
        loop {
            let mut buf = [0; SEQUENCE_ELEM_SIZE];
            reader.read_exact(&mut buf)?;
            buffer.extend_from_slice(&buf);
            if buf == NULL_TERMINATOR {
                break;
            }
        }
        Ok(())
    }

    /// Exposes a reference to each position and associated evaluation in the game sequentially, via a callback.
    pub fn visit_positions(&self, mut callback: impl FnMut(&Board, i32)) {
        let (mut board, _, _, _) = self.initial_position.unpack();
        for (mv, eval) in &self.moves {
            let eval = eval.get();
            callback(&board, i32::from(eval));
            board.make_move_simple(*mv);
        }
    }

    /// Internally counts how many positions would pass the filter in this game.
    pub fn filter_pass_count(&self, filter: &Filter) -> u64 {
        self.filter_pass_count_with_filter_callback(|mv, eval, board, outcome, rng| {
            filter.should_filter(mv, eval, board, outcome, rng)
        })
    }

    /// Internally counts how many positions would pass the filter in this game.
    pub fn filter_pass_count_with_filter_callback(
        &self,
        mut should_filter: impl FnMut(Move, i32, &Board, WDL, &mut ThreadRng) -> bool,
    ) -> u64 {
        let mut rng = rand::rng();
        let mut cnt = 0;
        let (mut board, _, wdl, _) = self.initial_position.unpack();
        let outcome = WDL::from_packed(wdl);
        for (mv, eval) in &self.moves {
            let eval = eval.get();
            if !should_filter(*mv, i32::from(eval), &board, outcome, &mut rng) {
                cnt += 1;
            }
            board.make_move_simple(*mv);
        }

        cnt
    }

    /// Converts the game into a sequence of marlinformat `PackedBoard` objects, yielding only those positions that pass the filter.
    pub fn splat_to_marlinformat(
        &self,
        callback: impl FnMut(marlinformat::PackedBoard) -> anyhow::Result<()>,
        filter: &Filter,
    ) -> anyhow::Result<()> {
        self.splat_to_marlinformat_with_filter_callback(
            callback,
            |mv, eval, board, outcome, rng| filter.should_filter(mv, eval, board, outcome, rng),
        )
    }

    /// Converts the game into a sequence of marlinformat `PackedBoard` objects, yielding only those positions that pass the filter.
    pub fn splat_to_marlinformat_with_filter_callback(
        &self,
        mut callback: impl FnMut(marlinformat::PackedBoard) -> anyhow::Result<()>,
        mut should_filter: impl FnMut(Move, i32, &Board, WDL, &mut ThreadRng) -> bool,
    ) -> anyhow::Result<()> {
        let mut rng = rand::rng();
        let (mut board, _, wdl, _) = self.initial_position.unpack();
        let outcome = WDL::from_packed(wdl);

        // record all the positions that pass the filter.
        for (mv, eval) in &self.moves {
            let eval = eval.get();
            if !should_filter(*mv, i32::from(eval), &board, outcome, &mut rng) {
                let marlinformat = board.to_marlinformat(eval, wdl, 0);
                callback(marlinformat)?;
            }
            board.make_move_simple(*mv);
        }

        Ok(())
    }

    /// Converts the game into a sequence of bulletformat `ChessBoard` objects, yielding only those positions that pass the filter.
    pub fn splat_to_bulletformat(
        &self,
        callback: impl FnMut(bulletformat::ChessBoard) -> anyhow::Result<()>,
        filter: &Filter,
    ) -> anyhow::Result<()> {
        self.splat_to_bulletformat_with_filter_callback(
            callback,
            |mv, eval, board, outcome, rng| filter.should_filter(mv, eval, board, outcome, rng),
        )
    }

    /// Converts the game into a sequence of bulletformat `ChessBoard` objects, yielding only those positions that pass the filter.
    pub fn splat_to_bulletformat_with_filter_callback(
        &self,
        mut callback: impl FnMut(bulletformat::ChessBoard) -> anyhow::Result<()>,
        mut should_filter: impl FnMut(Move, i32, &Board, WDL, &mut ThreadRng) -> bool,
    ) -> anyhow::Result<()> {
        let mut rng = rand::rng();
        let (mut board, _, wdl, _) = self.initial_position.unpack();
        let outcome = WDL::from_packed(wdl);

        // record all the positions that pass the filter.
        for (mv, eval) in &self.moves {
            let eval = eval.get();
            if !should_filter(*mv, i32::from(eval), &board, outcome, &mut rng) {
                let bulletformat = board.to_bulletformat(wdl, eval)?;
                callback(bulletformat)?;
            }
            board.make_move_simple(*mv);
        }

        Ok(())
    }

    /// Efficiency method that allows us to recover the move vector without allocating a new vector.
    pub fn into_move_buffer(self) -> Vec<(Move, marlinformat::util::I16Le)> {
        self.moves
    }
}

#[allow(clippy::unwrap_used)]
#[cfg(test)]
mod tests {
    use crate::chess::{piece::Colour, CHESS960};

    use super::*;

    use crate::chess::types::Square;

    #[test]
    #[ignore]
    fn roundtrip() {
        fn check_eq(lhs: &Board, rhs: &Board, msg: &str) {
            assert_eq!(
                lhs.pieces.all_pawns(),
                rhs.pieces.all_pawns(),
                "pawn square-sets {msg}"
            );
            assert_eq!(
                lhs.pieces.all_knights(),
                rhs.pieces.all_knights(),
                "knight square-sets {msg}"
            );
            assert_eq!(
                lhs.pieces.all_bishops(),
                rhs.pieces.all_bishops(),
                "bishop square-sets {msg}"
            );
            assert_eq!(
                lhs.pieces.all_rooks(),
                rhs.pieces.all_rooks(),
                "rook square-sets {msg}"
            );
            assert_eq!(
                lhs.pieces.all_queens(),
                rhs.pieces.all_queens(),
                "queen square-sets {msg}"
            );
            assert_eq!(
                lhs.pieces.all_kings(),
                rhs.pieces.all_kings(),
                "king square-sets {msg}"
            );
            assert_eq!(
                lhs.pieces.occupied_co(Colour::White),
                rhs.pieces.occupied_co(Colour::White),
                "white square-sets {msg}"
            );
            assert_eq!(
                lhs.pieces.occupied_co(Colour::Black),
                rhs.pieces.occupied_co(Colour::Black),
                "black square-sets {msg}"
            );
            for sq in Square::all() {
                assert_eq!(lhs.piece_at(sq), rhs.piece_at(sq), "piece_at({sq:?}) {msg}");
            }
            assert_eq!(lhs.turn(), rhs.turn(), "side {msg}");
            assert_eq!(lhs.ep_sq(), rhs.ep_sq(), "ep_sq {msg}");
            assert_eq!(
                lhs.castling_rights(),
                rhs.castling_rights(),
                "castle_perm {msg}"
            );
            assert_eq!(
                lhs.fifty_move_counter(),
                rhs.fifty_move_counter(),
                "fifty_move_counter {msg}"
            );
            assert_eq!(lhs.ply(), rhs.ply(), "ply {msg}");
            assert_eq!(lhs.all_keys(), rhs.all_keys(), "key {msg}");
            assert_eq!(lhs.threats(), rhs.threats(), "threats {msg}");
            assert_eq!(lhs.height(), rhs.height(), "height {msg}");
        }
        CHESS960.store(true, std::sync::atomic::Ordering::SeqCst);
        // Grab `valid.sfens` from `cozy-chess` to run test
        for sfen in include_str!("valid.sfens").lines() {
            let board = Board::from_fen(sfen).unwrap();
            let packed = marlinformat::PackedBoard::pack(&board, 0, 0, 0);
            let (unpacked, _, _, _) = packed.unpack();
            check_eq(&board, &unpacked, sfen);
        }
    }

    #[test]
    fn game_roundtrip() {
        let mut game = Game::new(&Board::default());
        game.add_move(Move::new(Square::E2, Square::E4), 0);
        game.add_move(Move::new(Square::E7, Square::E5), -314);
        game.add_move(Move::new(Square::G1, Square::F3), 200);

        let mut buf = Vec::new();
        game.serialise_into(&mut buf).unwrap();
        let game2 = Game::deserialise_from(&mut buf.as_slice(), Vec::new()).unwrap();
        assert_eq!(game.initial_position, game2.initial_position);
        assert_eq!(game.moves, game2.moves);
    }

    #[test]
    fn splat() {
        let mut game = Game::new(&Board::default());
        game.add_move(Move::new(Square::E2, Square::E4), 3);
        game.add_move(Move::new(Square::E7, Square::E5), -314);
        game.add_move(Move::new(Square::G1, Square::F3), 200);

        let mut boards = arrayvec::ArrayVec::<_, 32>::new();
        let filter = Filter::UNRESTRICTED;
        game.splat_to_marlinformat(
            |board| {
                boards.push(board);
                Ok(())
            },
            &filter,
        )
        .unwrap();
        assert_eq!(boards.len(), 3);
        let mut check_board = Board::default();
        assert_eq!(boards[0].unpack().0.to_string(), check_board.to_string());
        assert_eq!(boards[0].unpack().1, 3);
        assert!(check_board.make_move_simple(Move::new(Square::E2, Square::E4)));
        assert_eq!(boards[1].unpack().0.to_string(), check_board.to_string());
        assert_eq!(boards[1].unpack().1, -314);
        assert!(check_board.make_move_simple(Move::new(Square::E7, Square::E5)));
        assert_eq!(boards[2].unpack().0.to_string(), check_board.to_string());
        assert_eq!(boards[2].unpack().1, 200);
    }
}
