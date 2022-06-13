# stego-minesweeper
## Project scope
This project is based on [1], where researchers describe the usage of the “Minesweeper” game as the steganography tool. 
Our team will focus on the game itself and on the proposed way of encoding messages as the grid of mines.

## Minimal requirements (Stage 1)
1. Playable “Minesweeper” game with some hard-coded message and preferences (number of mines and board size):
   1. player can win or lose;
   2. player can do only valid moves;
   3. all game mechanics work correctly (e.g. number of neighbor mines).

## Additional features (Stage 2)
1. Converting any message (ASCII string) to the grid of mines for the game.
2. Menu (configurable preferences).
3. Client-server architecture (i.e. one user can create a game with a hidden message and share it with another user).
4. Separate utility for decoding the secret message from the game grid.

## References
[1]S. Mahato, D. Yadav and D. Khan, "A minesweeper game-based steganography scheme", 
Journal of Information Security and Applications, vol. 32, pp. 1-14, 2017 
[Online]. Available: https://www.sciencedirect.com/science/article/pii/S2214212616303064. 
[Accessed: 05 Jun. 2022]

