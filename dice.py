#! env python3
# -*- coding: utf-8 -*-

# Author: Avi Knoll (gz.ajknoll@gmail.com)

import random
import argparse
import re

def roll(die):
    results = dict()
    results['rolls'] = [random.randint(1, die['size'])
                        for i in range(die['count'])]
    total = sum(results['rolls'])
    for f in die['mods']:
        total = f(total)
    results['total'] = total
    return results

# Quick and dirty die string parser.
# TODO write a proper multistage parser.
def complexDice(string):
    pattern = re.compile("\s*(?:" + 
            "(\d+x)" +
            "|(\d+)" +
            "|(d)" + 
            "|([+\-/\*]\s*\d+)" +
            ")")
    scan = pattern.scanner(string)

    die = dict()
    die['string'] = string
    die['mods']   = []
    die['count']  = 1    # default value if none is supplied
    die['repeat'] = 1

    nextNum = 'count'
    m = scan.match()
    while m:
        value = repr(m.group(m.lastindex)).strip("'")
        print value
        if m.lastindex == 1:
            die['repeat'] *= int(value[:-1])
        elif m.lastindex == 4:
            die['mods'].append(generateModFunc(value[0], int(value[1:])))
        elif m.lastindex == 3:
            die['type'] = value
            nextNum = 'size'
        elif m.lastindex == 2:
            die[nextNum] = int(value)
            if nextNum == 'count':
                nextNum = 'size'

        m = scan.match()

    return die

def generateModFunc(operator, operand):
    if (operator == '+'):
        func = lambda x: x + operand
    elif (operator == '-'):
        func = lambda x: x - operand
    elif (operator == '*'):
        func = lambda x: x * operand
    elif (operator == '/'):
        func = lambda x: x / operand

    return func

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
