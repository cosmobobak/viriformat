viriformat is a crate for the game data representation used by the viridithas chess engine.

## Specification

All integers in viriformat are little-endian.

The square indexing used represents A1=0, H1=7, A8=56, H8=63.

A viriformat file consists of one or more `Game`s concatenated together.

A `Game` consists of a marlinformat `PackedBoard` followed by zero or more `Move` and `Score` pairs, terminated by four zero bytes.

A `PackedBoard` is a structure of:
- A 64-bit occupied-piece bitboard.
- A 32-entry array of 4-bit pieces, where the `i`th entry corresponds to the `i`th least-significant set bit in the occupied-piece bitboard.
  - The lower three bits of a piece corresponds to its type: pawn is 0, knight is 1, bishop is 2, rook is 3, queen is 4, king is 5.
  - Castling rights are represented by setting the piece type of the relevant rook (e.g. in classical chess, the a1 rook for queenside, or the h1 rook for kingside) to 6 to represent an unmoved rook.
  - A piece type of 7 is never valid.
  - A piece has its most-significant bit clear if it is a white piece, and set if it is a black piece.
  - Nonexistent piece entries may be left at zero.
- An 8-bit side-to-move and en-passant field.
  - The lower seven bits represent the en-passant target square, or 64 if there is no such square.
  - The most-significant bit is clear if white is to move, and set if black is to move.
- An 8-bit halfmove clock.
  - (This field may be left at zero.)
- A 16-bit fullmove counter, which must be non-zero.
  - (This field may be left at one.)
- A `Score` for the position.
  - (This field may be left at zero.)
- An 8-bit game-result field; a black win is 0, a draw is 1, a white win is 2. No other values are valid.
- An unused extra byte.

A `Move` is a structure packed into a 16-bit integer:
- A 6-bit from square.
- A 6-bit to square.
  - Castling moves are represented as king-takes-rook; a classical chess kingside castling would be represented as "E1 to H1". This is done to support Chess960.
- A 2-bit promotion piece, where knight is 0, bishop is 1, rook is 2, queen is 3.
  - If not a promotion move, this field may be left at zero.
- A 2-bit move type, where en-passant captures are 1, castling moves are 2, promotions (including capture-promotions) are 3. Any move not in the stated categories has a move type of 0.

A `Score` is a signed 16-bit integer representing a white-relative score for said `Move`.

# Example

```
00000000: ffff 0000 0000 ffff                       ; occupancy: startpos layout
00000008: 6124 5216 0000 0000 8888 8888 e9ac da9e   ; pieces: startpos layout
00000018: 40                                        ; side to move: white
                                                    ; en-passant: none
00000019: 00                                        ; halfmove clock: 0
0000001a: 0100                                      ; fullmove counter: 1
0000001c: 0000                                      ; position score: 0 cp
0000001e: 02                                        ; game result: white win
0000001f: 00                                        ; extra byte
00000020: 0c07 0a00                                 ; 1. e4       +10cp
00000024: 3409 1400                                 ; 1. ... e5   +20cp
00000028: c309 e2ff                                 ; 2. Qh5      -30cp
0000002c: 3c0d ff7f                                 ; 2. ... Ke7  +32767cp
00000030: 2709 ff7f                                 ; 3. Qxe5#    +32767cp
00000034: 0000 0000                                 ; terminator
```
