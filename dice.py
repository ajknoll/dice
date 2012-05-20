import random
import argparse
import re

def roll(die):
    rollDict = dict()
    rollDict['rolls'] = [random.randint(1, die['size'])
                         for i in range(die['count'])]
    total = sum(rollDict['rolls'])
    print total
    for f in die['mods']:
        total = f(total)
    rollDict['total'] = total
    return rollDict

# Quick and dirty die string parser.
# TODO write a proper multistage parser.
def complexDice(string):
    pattern = re.compile("\s*(?:(\d+)|(d)|([+\-/\*]\d+))")
    scan = pattern.scanner(string)

    die = dict()
    die['string'] = string
    die['mods']   = []
    die['count']  = 1    # default value if none is supplied

    nextNum = 'count'
    m = scan.match()
    while m:
        value = repr(m.group(m.lastindex)).strip("'")
        if m.lastindex == 3:
            y = int(value[1:])
            if (value[0] == '+'):
                func = lambda x: x + y
            elif (value[0] == '-'):
                func = lambda x: x - y
            elif (value[0] == '*'):
                func = lambda x: x * y
            elif (value[0] == '/'):
                func = lambda x: x / y
            die['mods'].append(func)
            print die['mods'][0](1)
        elif m.lastindex == 2:
            die['type'] = value
            nextNum = 'size'
        elif m.lastindex == 1:
            die[nextNum] = int(value)

        m = scan.match()

    return die

def _main():
    parser = argparse.ArgumentParser(
            description = 'Roll dice for tabletop games.')
    parser.add_argument(
            'dice',
            metavar = 'dice',
            type = complexDice,
            nargs = '+')

    args = parser.parse_args()
    for r, die in zip(map(roll, args.dice), args.dice):
        print die['string'], ":", r['total'], ":", r['rolls']
        

if __name__ == "__main__":
    _main()
