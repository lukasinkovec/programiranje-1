# 3.naloga

from functools import lru_cache

test1 = [2, 4, 1, 2, 1, 3, 1, 1, 5]
test2 = [4, 1, 8, 2, 11, 1, 1, 1, 1, 1]
test3 = [10 for n in range(50)]

def najmanj_skokov(mocvara, energija=0):
    if mocvara == []:
        return 0
    else:
        energija += mocvara[0]
        return min([(1 + najmanj_skokov(mocvara[(skok + 1):], (energija - skok - 1))) for skok in range(energija)])