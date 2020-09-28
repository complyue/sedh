__all__ = [
    "m_range",
    "n_range",
    "exclude",
]


def m_range():
    for m in range(5, 10):
        yield m

def n_range():
    for n in range(3, 8):
        yield n

def exclude(m, n):
    return abs(m - n) <= 2

