#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Logelei (Source: http://www.zeit.de/2016/52/spiele-logelei-52)
#
# ┏━┯━┳━┯━┳━┯━┓  horizontal:
# ┃A│B┃C│D┃E│F┃  A: digit sum of horizontal C, C: prime number, E: palindrome,
# ┠─┼─╀─╁─╀─┼─┨  G: multiple of the backward number of horizontal A,
# ┃G│ │ ┃H│ │ ┃  H: all digits are equal, I: vertical F times vertical K,
# ┠─┼─┾━╃─┾━┽─┨  L: multiple of vertical M, N: multiple of horizontal Q,
# ┃I│ │J│ │K│ ┃  P: vertical B is a multiple, Q: square number,
# ┣━┽─┼─┼─┼─┾━┫  R: square number, S: prime number.
# ┃L│ │ │ │ │M┃
# ┠─┾━┽─╆━┽─┼─┨  vertical:
# ┃N│O│ ┃P│ │ ┃  All numbers are square numbers.
# ┠─┼─╁─╀─╁─┼─┨
# ┃Q│ ┃R│ ┃S│ ┃
# ┗━┷━┻━┷━┻━┷━┛
#
# Attention: Code runs roughly 10 minutes on an old MacBook Pro 16GB Ram

import math
from pyDatalog import pyDatalog

pyDatalog.create_terms('math')
pyDatalog.create_terms('divmod')


@pyDatalog.program()
def _():
    is_divisible(X, Y) <= (divmod(X, Y)[1] == 0)

    _is_prim(X, Y) <= is_divisible(X, Y)
    _is_prim(X, Y) <= (X > Y+1) & _is_prim(X, Y+1)
    +is_prim(2)
    is_prim(X) <= (X > 2) & ~_is_prim(X, 2)

    _is_squared(X, Y) <= (X == Y**2)
    _is_squared(X, Y) <= (Y <= math.sqrt(X)) & _is_squared(X, Y+1)
    is_squared(X) <= _is_squared(X, 1)

    digits2num[A, B] = 10*A + B
    digits2num[A, B, C] = 10*digits2num[A, B] + C
    digits2num[A, B, C, D] = 10*digits2num[A, B, C] + D
    digits2num[A, B, C, D, E] = 10*digits2num[A, B, C, D] + E
    digits2num[A, B, C, D, E, F] = 10*digits2num[A, B, C, D, E] + F

    # Rows are denoted with A,B,C,D,E,F
    # Columns are denoted with 0,1,2,3,4,5

    # upper left corner
    ul(A0, A1, A2, A3, B0, B1, B2, C0, C1, D1) <= (
        # C horizontal
        A2.in_(range(1, 10)) & A3.in_(range(1, 10)) & is_prim(digits2num[A2, A3]) &
        # A horizontal
        A0.in_(range(1, 10)) & A1.in_(range(1, 10)) & (digits2num[A0, A1] == A2 + A3) &
        # C vertical
        B2.in_(range(10)) & is_squared(digits2num[A2, B2]) &
        # G horizontal
        B0.in_(range(1, 10)) & B1.in_(range(10)) & is_divisible(digits2num[B0, B1, B2], digits2num[A1, A0]) &
        # A vertical
        C0.in_(range(1, 10)) & is_squared(digits2num[A0, B0, C0]) &
        # B vertical
        C1.in_(range(10)) & D1.in_(range(10)) & is_squared(digits2num[A1, B1, C1, D1]))

    # upper right corner
    ur(A4, A5, B3, B4, B5, C5) <= (
        # E horizontal
        A4.in_(range(1, 10)) & A5.in_(range(1, 10)) & (A4 == A5) &
        # H horizontal
        B3.in_(range(1, 10)) & B4.in_(range(10)) & B5.in_(range(10)) & (B3 == B4) & (B4 == B5) &
        # E vertical
        C5.in_(range(10)) & is_squared(digits2num[A4, B5]) &
        # F vertical
        is_squared(digits2num[A5, B5, C5]))

    # lower left corner
    ll(D0, E0, E1, E2, F0, F1) <= (
        # Q horizontal
        F0.in_(range(1, 10)) & F1.in_(range(10)) & is_squared(digits2num[F0, F1]) &
        # O vertical
        E1.in_(range(1, 10)) & is_squared(digits2num[E1, F1]) &
        # N horizontal
        E0.in_(range(1, 10)) & E2.in_(range(10)) & is_divisible(digits2num[E0, E1, E2], digits2num[F0, F1]) &
        # L vertical
        D0.in_(range(1, 10)) & is_squared(digits2num[D0, E0, F0]))

    # lower right corner
    lr(A0, A1, A2, A3, B0, B1, B2, C0, C1, C4, D1, D4, D5, E3, E4, E5, F2, F3, F4, F5) <= (
        # fulfill upper left corner in order to have B vertical
        ul(A0, A1, A2, A3, B0, B1, B2, C0, C1, D1) &
        # S horizontal
        F4.in_(range(1, 10)) & F5.in_(range(10)) & is_prim(digits2num[F4, F5]) &
        # M vertical
        D5.in_(range(1, 10)) & E5.in_(range(10)) & is_squared(digits2num[D5, E5, F5]) &
        # P vertical
        E3.in_(range(1, 10)) & F3.in_(range(10)) & is_squared(digits2num[E3, F3]) &
        # P horizontal
        E4.in_(range(10)) & is_divisible(digits2num[A1, B1, C1, D1], digits2num[E3, E4, E5]) &
        # R horizontal
        F2.in_(range(1, 10)) & is_squared(digits2num[F2, F3]) &
        # K vertical
        C4.in_(range(1, 10)) & D4.in_(range(10)) & is_squared(digits2num[C4, D4, E4, F4]))

    # complete board
    board(X) <= (
        # fulfill all corners and connect them
        ul(X[0][0], X[0][1], X[0][2], X[0][3], X[1][0], X[1][1], X[1][2], X[2][0], X[2][1], X[3][1]) &
        ur(X[0][4], X[0][5], X[1][3], X[1][4], X[1][5], X[2][5]) &
        lr(X[0][0], X[0][1], X[0][2], X[0][3], X[1][0], X[1][1], X[1][2], X[2][0], X[2][1], X[2][4],
           X[3][1], X[3][4], X[3][5], X[4][3], X[4][4], X[4][5], X[5][2], X[5][3], X[5][4], X[5][5]) &
        ll(X[3][0], X[4][0], X[4][1], X[4][2], X[5][0], X[5][1]) &
        X[2][2].in_(range(1, 10)) & X[2][3].in_(range(10)) &
        # I horizontal
        (I == digits2num[X[2][0], X[2][1], X[2][2], X[2][3], X[2][4], X[2][5]]) &
        (F == digits2num[X[0][5], X[1][5], X[2][5]]) &
        (K == digits2num[X[2][4], X[3][4], X[4][4], X[5][4]]) &
        (I == F*K) &
        X[3][3].in_(range(1, 10)) &
        # D vertical
        is_squared(digits2num[X[0][3], X[1][3], X[2][3], X[3][3]]) &
        X[3][2].in_(range(1, 10)) &
        # L horizontal
        (L == digits2num[X[3][0], X[3][1], X[3][2], X[3][3], X[3][4], X[3][5]]) &
        (M == digits2num[X[3][5], X[4][5], X[5][5]]) &
        is_divisible(L, M) &
        # J vertical
        is_squared(digits2num[X[2][2], X[3][2], X[4][2], X[5][2]]))

    print(board([(A0, A1, A2, A3, A4, A5), (B0, B1, B2, B3, B4, B5), (C0, C1, C2, C3, C4, C5),
                 (D0, D1, D2, D3, D4, D5), (E0, E1, E2, E3, E4, E5), (F0, F1, F2, F3, F4, F5)]))
