##############################################################################
# Želimo definirati pivotiranje na mestu za tabelo [a]. Ker bi želeli
# pivotirati zgolj dele tabele, se omejimo na del tabele, ki se nahaja med
# indeksoma [start] in [end].
#
# Primer: za [start = 0] in [end = 8] tabelo
#
# [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# preuredimo v
#
# [0, 2, 5, 4, 10, 11, 17, 15, 18]
#
# (Možnih je več različnih rešitev, pomembno je, da je element 10 pivot.)
#
# Sestavi funkcijo [pivot(a, start, end)], ki preuredi tabelo [a] tako, da bo
# element [ a[start] ] postal pivot za del tabele med indeksoma [start] in
# [end]. Funkcija naj vrne indeks, na katerem je po preurejanju pristal pivot.
# Funkcija naj deluje v času O(n), kjer je n dolžina tabele [a].
#
# Primer:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot(a, 1, 7)
#     3
#     >>> a
#     [10, 2, 0, 4, 11, 15, 17, 5, 18]
##############################################################################

def swap(list, i, j):
    x = list[i]
    list[i] = list[j]
    list[j] = x

def pivot(a, start, end):
    if len(a) == 1:
        return 0
    else:
        p, less = a[start], start + 1
        while True:
            more = less
            while (a[less] < p) and (more < end):
                less += 1
                more += 1
            while (a[more] > p) and (more < end):
                more += 1
            if more == end:
                if a[more] < p:
                    swap(a, less, more)
                    swap(a, start, less)
                    return less
                else:
                    swap(a, start, less - 1)
                    return less - 1
            swap(a, less, more)
            less += 1

##############################################################################
# Tabelo a želimo urediti z algoritmom hitrega urejanja (quicksort).
#
# Napišite funkcijo [quicksort(a)], ki uredi tabelo [a] s pomočjo pivotiranja.
# Poskrbi, da algoritem deluje 'na mestu', torej ne uporablja novih tabel.
#
# Namig: Definirajte pomožno funkcijo [quicksort_part(a, start, end)], ki
#        uredi zgolj del tabele [a].
#
#   >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#   >>> quicksort(a)
#   [2, 3, 4, 5, 10, 11, 15, 17, 18]
##############################################################################

def quicksort_part(a, start, end):
    if start >= end:
        return None
    else:
        x = pivot(a, start, end)
        quicksort_part(a, start, x - 1)
        quicksort_part(a, x + 1, end)

def quicksort(a):
    quicksort_part(a, 0, len(a) - 1)
    return a

##############################################################################
# V tabeli želimo poiskati vrednost k-tega elementa po velikosti.
#
# Primer: Če je
#
# >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#
# potem je tretji element po velikosti enak 5, ker so od njega manši elementi
#  2, 3 in 4. Pri tem štejemo indekse od 0 naprej, torej je "ničti" element 2.
#
# Sestavite funkcijo [kth_element(a, k)], ki v tabeli [a] poišče [k]-ti
# element po velikosti. Funkcija sme spremeniti tabelo [a]. Cilj naloge je, da
# jo rešite brez da v celoti uredite tabelo [a].
##############################################################################

def kth_element_part(a, k, start, end):
    if start >= end:
        return None
    else:
        x = pivot(a, start, end)
        if x == k:
            return None
        elif x > k:
            kth_element_part(a, k, start, x - 1)
        else:
            kth_element_part(a, k, x + 1, end)

def kth_element(a, k):
    kth_element_part(a, k, 0, len(a) - 1)
    return a[k]