The data types supported by the machine consist only of variables in which natural numerals can be stored.
The set of variables is partitioned into 
input variables x_1... x_n
output variable y
local variables: z_1 ... z_m
Instructions Supported
Inc v  (increment variable v by 1, and then go to the next instruction in the program)
Dec v (if non-zero, decrement the variable v by 1 else do nothing.  Then go to next instruction in the program)
Jnz v L  (test if variable v has a non-zero value, and if so, jump to the instruction labelled L, otherwise go to next isntruction in the program)
Nop  (do nothing, go to next instruction in the program).

The end of the program is marked by a distinguished label, and contains a "pseudo instruction" Halt.  
Program execution halts, and the value of the output variable is displayed.

Implemented a series of macros for 
unconditional jumps  (GOTO L)
copying a value from one variable to another;  (V := V')
initializing a variable to a constant.  In particular to 0;  (V := c)
adding two variables V := V1+v2
multiplying two variables  V:= V1 * V2
checking if one variable is greater than another   V1 >= V2
Subtracting a variable from another, and looping if the second is larger than the first V := V1 - V2
implementing a general macro expansion of a known computation within another.  w := f(v_1,...,v_n)
Boolean tests and conditional jumps.  If p(v1,...,vn) GOTO L

Parsing through lexer in A2.mll.