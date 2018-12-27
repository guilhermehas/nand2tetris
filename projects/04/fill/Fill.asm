// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

@LASTKEY
M=0
(START)
@SCREEN
D=A
@addr
M=D // addr = 16384

@8192
D=A
@n
M=D // n = NMAX
@i
M=0 // i = 0

(LOOP)
@i
D=M
@n
D=D-M
@END
D;JGT // if i>n goto END
@LASTKEY
D=M

@addr
A=M
M=D // RAM[addr]=1111111111111111

@i
M=M+1 // i = i + 1
@32
D=A
@addr
M=M+1 // addr = addr + 32
@LOOP
0;JMP // goto LOOP

(CHANGENEG)
@1111111111111111
D=A
@LASTKEY
M=D
@START
0;JMP

(END)
@KBD
D=M
@CHANGENEG
D;JNE

@0
D=A
@LASTKEY
M=D
@START
0;JMP
