jmp = """JGT 0 0 1
JEQ 0 1 0
JGE 0 1 1
JLT 1 0 0
JNE 1 0 1
JLE 1 1 0
JMP 1 1 1"""


def toArray():
    for it in jmp.split('\n'):
        jt = it.split(' ')
        s0 = jt[0]
        rv = ', '.join(jt[1:])
        yield f'("{s0}", [{rv}])'


def toString():
    sn = ', '.join(toArray())
    return f"[{sn}]"


print(toString())
