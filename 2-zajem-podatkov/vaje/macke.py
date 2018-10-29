import requests
import re
import os
import csv

###############################################################################
# Najprej definirajmo nekaj pomožnih orodij za pridobivanje podatkov s spleta.
###############################################################################

# definiratje URL glavne strani bolhe za oglase z mačkami
cats_frontpage_url = 'http://www.bolha.com/zivali/male-zivali/macke/'
# mapa, v katero bomo shranili podatke
cat_directory = 'cat_data'
# ime datoteke v katero bomo shranili glavno stran
frontpage_filename = 'frontpage.html'
# ime CSV datoteke v katero bomo shranili podatke
csv_filename = 'cat_data.csv'


def download_url_to_string(url):
    '''This function takes a URL as argument and tries to download it
    using requests. Upon success, it returns the page contents as string.'''
    try:
        # del kode, ki morda sproži napako
        r = requests.get(url)
    except requests.exceptions.ConnectionError:
        # koda, ki se izvede pri napaki
        print("Could not access page " + url)
        # dovolj je če izpišemo opozorilo in prekinemo izvajanje funkcije
        return ""
    # nadaljujemo s kodo če ni prišlo do napake
    return r.text


def save_string_to_file(text, directory, filename):
    '''Write "text" to the file "filename" located in directory "directory",
    creating "directory" if necessary. If "directory" is the empty string, use
    the current directory.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None

# Definirajte funkcijo, ki prenese glavno stran in jo shrani v datoteko.


def save_frontpage():
    '''Save "cats_frontpage_url" to the file
    "cat_directory"/"frontpage_filename"'''
    text = download_url_to_string(cats_frontpage_url)
    save_string_to_file(text, cat_directory, frontpage_filename)
    return None

###############################################################################
# Po pridobitvi podatkov jih želimo obdelati.
###############################################################################


def read_file_to_string(directory, filename):
    '''Return the contents of the file "directory"/"filename" as a string.'''
    seznam = []
    vzorec = re.compile(r'<div class="ad featured">'
             r'<div class="coloumn image">'
             r'<table><tr><td><a title="pasemska britanka britanke s FIFE slovenskim rodovnikom" href="http://www.bolha.com/bs/r/PUJoZUPNTx2kTSSOvY-4DfQ34DYYo_8gmvgNZm8TR7ocOTCVHhdNehVxiVSepVQJbvT78J3Nwv475wTT-vctJHk9Ud-Idp_MranJRWzUrxTsotLbcIzajrKqMKpRChQTJxRhiH1M3SJk-YfLgKbDe1VpeQTryul9jc5E5ZuXazkPgIuaw7ROv7b_P5Qz1ikG9yW9Hw0.?adId=1339050872&amp;clkType=organic&amp;dst=internal&amp;source=listing&amp;section=organic&amp;category=%C5%BDivali%2FMale+%C5%BEivali%2FMa%C4%8Dke%2FMa%C4%8Dke+z+rodovnikom%2F&amp;viewType=30&amp;action=imgClk&amp;target=adDetail&amp;redirectFrom=http://www.bolha.com/zivali/male-zivali/macke&amp;redirectTo=http://www.bolha.com//zivali/male-zivali/macke/macke-z-rodovnikom/pasemska-britanka-britanke-s-fife-slovenskim-rodovnikom-1339050872.html&amp;ts=1539595331"><img src="https://mmc.bolha.com/storage/2/thumb2/4f64/0bf0/pasemskabritankabritankesfifeslorodovnikom-1000.png" alt="pasemska britanka britanke s FIFE slovenskim rodovnikom" /></a></td></tr></table>'
             r'<span class="flag_newAd"></span>                        </div>'
             r'<div class="coloumn content">'
             r'<h3><a title="pasemska britanka britanke s FIFE slovenskim rodovnikom" href="http://www.bolha.com/bs/r/PUJoZUPNTx2kTSSOvY-4DfQ34DYYo_8gmvgNZm8TR7ocOTCVHhdNehVxiVSepVQJbvT78J3Nwv475wTT-vctJHk9Ud-Idp_MranJRWzUrxTsotLbcIzajrKqMKpRChQTJxRhiH1M3SJk-YfLgKbDe1VpeQTryul9jc5E5ZuXazkPgIuaw7ROv7b_P5Qz1ikG9yW9Hw0.?adId=1339050872&amp;clkType=organic&amp;dst=internal&amp;source=listing&amp;section=organic&amp;category=%C5%BDivali%2FMale+%C5%BEivali%2FMa%C4%8Dke%2FMa%C4%8Dke+z+rodovnikom%2F&amp;viewType=30&amp;action=titleClk&amp;target=adDetail&amp;redirectFrom=http://www.bolha.com/zivali/male-zivali/macke&amp;redirectTo=http://www.bolha.com//zivali/male-zivali/macke/macke-z-rodovnikom/pasemska-britanka-britanke-s-fife-slovenskim-rodovnikom-1339050872.html&amp;ts=1539595331">pasemska britanka britanke s FIFE slovenskim rodovnikom</a></h3>'
             r'pasma:britanska kratkodlaka barva /spol: crem bicolur samček skoten :23.8.2018, ime: Ike Kiki cattery *si oče: Lord Kiki cattery*si, modra mati: Haily                                                        <div class="additionalInfo"><span class="extraBadge"><a href="/pasemske-zivali">Z rodovnikom</a></span></div>                        </div>'
             r'<div class="coloumn badges">'
             r'&nbsp;                        </div>'
             r'<div class="coloumn prices">'
             r'<div class="price"><span>800,00 €</span></div>                        </div>'
             r'<div class="clear"></div>'
             r'<div class="miscellaneous">'
             r'<div class="coloumn saveAd itemVisibility certifiedUserPosition">'
             r'<a href="javascript:void(0);" data-id="1339050872" title="Shrani oglas" class="button btnYellow save _ad">Shrani oglas</a>'
             r'</div>'
             r'<p class="certifiedUserList"><a href="http://www.bolha.com/koristno/pomoc-in-varnost/preverjeni-uporabnik">Preverjeni uporabnik</a></p>'
             r'<div>'
             r'</div>'
             r'<div class="clear"></div>'
             r'</div>'
             r'</div>', re.DOTALL)
    for ujemanje in re.finditer(vzorec, frontpage_filename):
        seznam.append(ujemanje.group(0))
    return seznam

# Definirajte funkcijo, ki sprejme niz, ki predstavlja vsebino spletne strani,
# in ga razdeli na dele, kjer vsak del predstavlja en oglas. To storite s
# pomočjo regularnih izrazov, ki označujejo začetek in konec posameznega
# oglasa. Funkcija naj vrne seznam nizov.


def page_to_ads(TODO):
    '''Split "page" to a list of advertisement blocks.'''
    return TODO

# Definirajte funkcijo, ki sprejme niz, ki predstavlja oglas, in izlušči
# podatke o imenu, ceni in opisu v oglasu.


def get_dict_from_ad_block(TODO):
    '''Build a dictionary containing the name, description and price
    of an ad block.'''
    return TODO

# Definirajte funkcijo, ki sprejme ime in lokacijo datoteke, ki vsebuje
# besedilo spletne strani, in vrne seznam slovarjev, ki vsebujejo podatke o
# vseh oglasih strani.


def ads_from_file(TODO):
    '''Parse the ads in filename/directory into a dictionary list.'''
    return TODO

###############################################################################
# Obdelane podatke želimo sedaj shraniti.
###############################################################################


def write_csv(fieldnames, rows, directory, filename):
    '''Write a CSV file to directory/filename. The fieldnames must be a list of
    strings, the rows a list of dictionaries each mapping a fieldname to a
    cell-value.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)
    return None

# Definirajte funkcijo, ki sprejme neprazen seznam slovarjev, ki predstavljajo
# podatke iz oglasa mačke, in zapiše vse podatke v csv datoteko. Imena za
# stolpce [fieldnames] pridobite iz slovarjev.


def write_cat_ads_to_csv(TODO):
    return TODO
