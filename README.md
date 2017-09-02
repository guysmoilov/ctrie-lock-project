# Lock-based CTries
This is a fork of https://github.com/axel22/Ctries/

In this project, we attempt to do the following:
1. Replace the lock-free approach with a lock-based approach, and measure relative performance.
2. Potentially optimize the existing implemenation, regardless of locks, and achieve performance improvements.

Relevant code can be found in the code packages named ctrielock.
Link to a paper explaining our implementation and benchmarks is available at: https://github.com/guysmoilov/ctrie-lock-project/blob/master/lock-based-ctries.pdf
