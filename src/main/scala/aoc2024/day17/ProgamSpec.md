| Raw Input | Input | Meaning                      |
|-----------|-------|------------------------------|
| 2,4       | bst A | B = Lowest 3 bits of A (% 8) |
| 1,2       | bxl 2 | B = XOR(B, 0b010)            |
| 7,5       | cdv 5 | C = A >> B                   |
| 4,5       | bxc 5 | B = XOR(B, C)                |
| 1,3       | bxl 3 | B = XOR(B, 0b011)            |
| 5,5       | out 5 | print B % 8                  |
| 0,3       | adv 3 | A = A >> 3                   |
| 3,0       | jnz 0 | If A != 0, jump to 0         |

# Observations

1. The program goes back to 0 at the end if register A is not 0.
2. Every iteration, register A is shifted right by 3 bits.
3. Register A is not updated other than the shift right operation.
4. B is updated multiple times.
5. The program should end after 16 iterations. Register A should be 0 after 16 shift right by 3 bit operations. This means bits higher than the 48th bit should be 0.
6. We can first determine the bits 45 - 47 (0-based) because the last output (0) is determined by these bits only.
7. Once we determined bits 45 - 47, we can try to append another 3 bits to them to determine the second last output (3).
8. Continue to so until we reached the first output (2).