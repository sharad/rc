#!/usr/bin/python3
# -*- python-indent-offset: 4; -*-

import os
import sys, getopt
import string
import re

import PyPDF2
import tabula
from pprint import pprint
import pandas as pd
import xml.etree.cElementTree as ET

# global
debugTrace =  False

def main(pgm, argv):
    pages      = None
    try:
        opts, args = getopt.getopt(argv, "hp::", ['ppages='])
    except getopt.GetoptError:
        print(pgm, ' -p pages')
        sys.exit(2)

    for opt, arg in opts:
        if opt == '-h':
            print(pgm, ' -p pages')
            sys.exit()
        elif opt in ("-p", "--ppage"):
            pages = arg
    filesToXml( args )

def filesToXml(files): 
    table =  extractTable( files )
    table =  santizeTable( table )
    tableToXml( table )

def sanitizeXmlTag(tag):
    xmltransform =  str.maketrans(": ",  "xx")
    return tag.translate(xmltransform)

def tableToXml(table):
    account     = ET.Element("account")
    transactions = ET.SubElement(account, "transactions")
    for i, row in table.iterrows():
        transaction = ET.SubElement(transactions, "transaction")
        for col in table.columns:
            subEleCol =  ET.SubElement(transaction, sanitizeXmlTag( col ))
            subEleCol.text =  str( row[ col ] )
    tree = ET.ElementTree(account)
    tree.write( sys.stdout,
                encoding='unicode',
                xml_declaration=True,
                default_namespace=None,
                method="xml",
                # *,
                short_empty_elements=True)

def santizeTable(table):
    return table

def extractTable(files): 
    Table =  pd.DataFrame()
    for file in files:
        if os.path.exists( file ): 
            fileTable = extractTableInFile( file )
            Table =  pd.concat([Table,  fileTable], sort=False)
        else:
            raise NameError("file not exist",  file)
    return Table

    # if pages and isinstance(pages,  str):
    #    pages =  pages.lstrip("%").  split(",")
    #    pages =  list(map(float, pages))

def extractTableInFile(file): 
    lastTop = -1
    pages   = getPages(file)

    fileTable =  pd.DataFrame()

    for page in range(1, pages):
        debug("lastTop",  lastTop)
        lastCol = findcolumn(lastTop if 0 <= lastTop else  0,
                             100,
                             file,
                             page)
        if 0 != lastCol:
            lastTop =  bisection(lastTop if -1 < lastCol else  0,
                                 100     if -1 < lastCol else lastTop,
                                 findcolumn,
                                 file,
                                 page)
            debug("New lastTop",  lastTop)
        else: 
            debug("Not required to calculate lastTop",  lastTop)

        pgTable   = extractTableInPage(file,  page,  [lastTop,  0,  100,  100]);
        fileTable = pd.concat([fileTable, pgTable], sort=False)

    return fileTable

def getPages(file):
   # https://stackoverflow.com/questions/42538292/extracting-tables-from-pdfs-using-tabula
   reader = PyPDF2.PdfFileReader(open(file, mode='rb' ))
   return reader.getNumPages()


def findcolumn(min, max, file, page): 
   rows = tabula.read_pdf(file,
                          pages = [page],
                          area = [min, 0, max,  100],
                          relative_area =  True,
                          silent = True,
                          multiple_tables=True,
                          stream = True,
                          no_spreadsheet = True,
                          guess =  False,
                          lattice =  False,
                          pandas_options={
                           # 'header': None,
                           # 'error_bad_lines': False,
                           # 'warn_bad_lines': False
                       })
   for row in rows:
      for i, j in row.iterrows():
        if isinstance(j.values[0], str) and re.match( r'^Date', j.values[0], re.M|re.I): 
                return i
   return -1;


def bisection(min, max, method, pdf, page):
        median =  (max +  min) / 2
        minR =  method(min, median, pdf, page)
        if minR > -1:
                if minR ==  0:
                        return min
                else:
                        return bisection(min, median, method, pdf, page)
        maxR =  method(median, max, pdf, page)
        if maxR > -1:
                if maxR ==  0:
                        return median
                else:
                        return bisection(median, max,  method, pdf, page)


def extractTableInPage(file,  page,  area):
   if area and isinstance(area,  str):
      area =  area.lstrip("%").  split(",")
      area =  list(map(float, area))
   rows = tabula.read_pdf(file,
                          pages = [page],
                          area = area,
                          relative_area =  True,
                          silent = True,
                          multiple_tables=False,
                          stream = True,
                          no_spreadsheet = True,
                          guess =  False,
                          lattice =  False,
                          pandas_options={
                           # 'header': None,
                           # 'error_bad_lines': False,
                           # 'warn_bad_lines': False
                       })
   return rows



def debug( *args ):
    if True == debugTrace: 
        print( *args )



def extractpdf(file,  page,  area):
   if area and isinstance(area,  str):
      area =  area.lstrip("%").  split(",")
      area =  list(map(float, area))
   rows = tabula.read_pdf(file,
                          pages = [page],
                          area = area,
                          relative_area =  True,
                          silent = True,
                          multiple_tables=True,
                          stream = True,
                          no_spreadsheet = True,
                          guess =  False,
                          lattice =  False,
                          pandas_options={
                           # 'header': None,
                           # 'error_bad_lines': False,
                           # 'warn_bad_lines': False
                       })
   debug_first_columns =  []
   seen_date_once = False;

   for row in rows:
      print()
      print()
      print("type of row: ",  type(row))
      print("Printing next row:  ")
      print()
      print()
      print()


      seen_date = False;
      for i, j in row.iterrows():

         debug_first_columns.append(j.values[0])
         # print("j.values[0]:  ", j.values[0])

         if (j.values[0] ==  "Date"):
            seen_date =  True
            seen_date_once =  True

         if seen_date:
            if debug:
               print("type of returned value i: ",  type(i))
               print("value of i:  ",  i);
               print("type of returned value j: ",  type(j))
            print(j.values.tolist())
            print()



      if not(seen_date_once):
         print("Failed to get table try correcting area from {}\n".format(area))
         for col in debug_first_columns:
            print(col)





if __name__ == "__main__":
   main(sys.argv[0],  sys.argv[1:])
