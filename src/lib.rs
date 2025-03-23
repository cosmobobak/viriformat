pub mod chess;
pub mod dataformat;
mod lookups;
mod makemove;
mod rng;

#[cfg(test)]
mod tests {
    use std::io::BufReader;

    use crate::{chess::{board::Board, chessmove::{Move, MoveFlags}, types::Square}, dataformat::Game};

    #[test]
    fn from_ep_startpos_roundtrip() {
        const FEN: &str = "r1bqkbnr/pppp1pp1/2n5/4p3/4P1Pp/2N2N2/PPPP1P1P/R1BQKBR1 b Qkq g3 0 5";
        let board = Board::from_fen(FEN).unwrap();
        let mut game = Game::new(&board);
        game.add_move(Move::new_with_flags(Square::H4, Square::G3, MoveFlags::EnPassant), 1337);
        let mut memory = Vec::new();
        game.serialise_into(&mut memory).unwrap();
        let game2 = Game::deserialise_from(&mut BufReader::new(&*memory), Vec::new()).unwrap();
        assert_eq!(game, game2);
    }
}
