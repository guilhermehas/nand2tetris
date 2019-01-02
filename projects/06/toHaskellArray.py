elems = """0 1 0 1 0 1 0
1 1 1 1 1 1 1
-1 1 1 1 0 1 0
D 0 0 1 1 0 0
A M 1 1 0 0 0 0
!D 0 0 1 1 0 1
!A !M 1 1 0 0 0 1
-D 0 0 1 1 1 1
-A -M 1 1 0 0 1 1
D+1 0 1 1 1 1 1
A+1 M+1 1 1 0 1 1 1
D-1 0 0 1 1 1 0
A-1 M-1 1 1 0 0 1 0
D+A D+M 0 0 0 0 1 0
D-A D-M 0 1 0 0 1 1
A-D M-D 0 0 0 1 1 1
D&A D&M 0 0 0 0 0 0
D|A D|M 0 1 0 1 0 1"""


def getElems():
    for el in elems.split('\n'):
        el = el.split(' ')
        binEls = el[-6:]
        yield (el[0], ['0'] + binEls)
        if len(binEls) == 8:
            yield (el[1], ['1'] + binEls)


def getList():
    for st, array in getElems():
        arraySt = ', '.join(array)
        yield f'("{st}", [{arraySt}])'


def toHaskellArray():
    lista = list(getList())
    lista = ', '.join(lista)
    listan = f"[{lista}]"
    return listan


print(toHaskellArray())
