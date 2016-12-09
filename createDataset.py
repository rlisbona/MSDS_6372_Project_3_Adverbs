

# -*- coding: utf-8 -*-
"""
Created on Sun Nov  6 01:54:53 2016

@author: Joe Stoffa
"""
import nltk
import os
import json
import requests
from lxml import html
import pandas as pd

pd.set_option("display.max_columns", 30) #set to view all columns in dataset



#set directory for saving dataset
#parent = os.path.dirname(__file__)
parent = 'c:\\Users\\anobs\\Documents\\GitHub\\MSDS6372_Project3'
BookDir = os.path.join(parent, 'Books')
DataDir = os.path.join(parent, 'Data')

print(os.getcwd())

filename = os.path.join(DataDir, 'test.txt')


#scrapes dictionary.com to determine a words type
def lookupType(word):
    baseURL = "http://www.dictionary.com/browse/"
    url = baseURL + word
    removeWords = ["sentence", "idiom", "idioms", "interjection"]
    trans_table = dict.fromkeys(map(ord, ','), None)
    page = requests.get(url)
    tree = html.fromstring(page.content)

    wordTypesRaw = tree.xpath('//header[@class="luna-data-header"]//span[@class="dbox-pg"]/text()')

    wordTypes = []
    for word in wordTypesRaw:
        word = word.lower()
        word = word.translate(trans_table)
        wordTypes.append(word.split(" ")[0])
    
    wordTypes = set(wordTypes)
    for word in removeWords:
        if word in wordTypes:
            wordTypes.remove(word)
    
    return(list(wordTypes))

#takes a list of token lists and returns a lists of tokens found in all lists
def findShared(list_of_lists):
    notin = []
    seen = set(list_of_lists[0])
    for i in range(1, len(list_of_lists)):
        for w in seen:
            if w not in list_of_lists[i] and w not in notin:
                notin.append(w)
    for r in notin:
        seen.remove(r)
    return(seen)
          
#returns a dictionary of the number and percents of small, medium, and large
#words
def getWordDist(tokens):
    word_count = len(tokens)
    small_words = []
    med_words = []
    large_words = []
    for w in tokens:
        if len(w) < 4:
            small_words.append(w)
        elif len(w) < 6 and len(w) > 3:
            med_words.append(w)
        else:
            large_words.append(w)
    small = len(small_words)
    med = len(med_words)
    large = len(large_words)
    return({"Small": small, "Medium": med, "Large": large, \
            "PerSmall": format(small/word_count*100, ".2f"), \
            "PerMed": format(med/word_count*100, ".2f"), \
            "PerLarge": format(large/word_count*100, ".2f"), \
            "SmallList": small_words, "MedList": med_words, "LargeList": large_words})

#returns a dictionary that includes: list of common tokens and books.  Books is
#a list of books and contains: title, author, and dictionary of words and their
#frequency in the text
def createVocabDistro(directory):
    vocab_dict = {}
    files = os.listdir(directory)
    list_of_lists = []
    books = []

    for file in files:
        book = {}
        split_file = file.split("-")
        author = split_file[0]
        title = split_file[1]
        book_text = open(directory+"\\"+file, "r")
        tokens = nltk.word_tokenize(book_text.read())
        tokens = [w.lower() for w in tokens if w.isalpha()]
        fdist = nltk.FreqDist(tokens)
        freq_dict = getWordDist(tokens)
        vocabulary = set(tokens)
        list_of_lists.append(vocabulary)
        book["title"] = title
        book["author"] = author
        book["words"] = len(tokens)
        book["frequency"] = fdist
        book["perSmall"] = freq_dict["PerSmall"]
        book["perMed"] = freq_dict["PerMed"]
        book["perLarge"] = freq_dict["PerLarge"] 
        books.append(book)
        book_text.close()
        
    shared = list(findShared(list_of_lists))
    vocab_dict["shared"] = shared
    vocab_dict["books"] = books
    return(vocab_dict)


#write vocabDistro to file or if file already exists load from file
jsonfilename = os.path.join(DataDir, "vocabDistro.json")

if os.path.isfile(jsonfilename):
    print("reading stored information from json file")
    with open(jsonfilename) as json_data:
        d = json.load(json_data)
else:
    with open(jsonfilename, "w") as fout:
        d = createVocabDistro(BookDir)
        json.dump(d, fout)


#build dictionary from shared vocab words, if word is not already in
#dictionary.txt, scrape www.dictionary.com for word.  After all shared words
#are checked save dictionary to dicionary.txt if applicable.  Dictionary.txt
#is created so that www.dictionary.com server(s) aren't accessed more
#than necessary
dictFile = os.path.join(DataDir, "dictionary.json")
if os.path.isfile(dictFile):
    textfile = open(dictFile, "r")
    dictionary = json.load(textfile)
    textfile.close()
else:
    with open(dictFile, "w") as fout:
        fout.write("{}")
        dictionary = "{}"
        fout.close()
update = False
for word in d['shared']:
    if word not in dictionary:
        try:
            dictionary[word] = {"type": lookupType(word)}
            print("looking up "+word)
            update = True
        except:
            print("error looking up "+word)

if update:
    print("***updating dictionary***")
    fout = open(dictFile, "w")
    json.dump(dictionary, fout)
    fout.close()

print("Adverbs: ", len([w for w in d["shared"] if len(w) > 4 and "adverb"\
                       in dictionary[w]["type"]]))

#get list of adverbs from saved file if it exists or randomly sample 20 if it
#doesn't
adverb_filename = os.path.join(DataDir, "chosenAdverbs.txt")
if os.path.isfile(adverb_filename):
    with open(adverb_filename) as f:
        Words = f.read().split(", ")
else:
    #randomly chose 8 adverbs longer than 4 letters from word list and save
    #list to file
    with open(adverb_filename, "w") as fout:
        Words = [w for w in d["shared"] if len(w) > 4 and "adverb"\
                       in dictionary[w]["type"]]
        fout.write(', '.join(Words))

#create dataset from directory of books
Title = []
Author = []
Per_Small = []
Per_Medium = []
Per_Large = []

for book in d["books"]:
    Title.append(book["title"])
    Author.append(book["author"])
    Per_Small.append(book["perSmall"])
    Per_Medium.append(book["perMed"])
    Per_Large.append(book["perLarge"])

booksDF = pd.DataFrame.from_items([('Title',Title), ('Author',Author), ('Per_Small',Per_Small),\
                                   ('Per_Medium',Per_Medium), ('Per_Large',Per_Large)])
    
for word in Words:
    word_freq = []
    for book in d["books"]:
        word_freq.append(format(book["frequency"][word]/book["words"]*100, ".4f"))
    booksDF[word] = word_freq
        

print(Words)
print("shared: ", len(d["shared"]))

df_textfile = os.path.join(DataDir, "booksDF.csv")
if not (os.path.isfile(df_textfile)):
    print("***Writing Dataframe to CSV file***")
    booksDF.to_csv(df_textfile, index=False)
else:
    print("Dataframe is already stored as a file.")

