#!/usr/bin/python3
# -*- python-indent-offset: 4; -*-

import os
import sys, getopt
import string
import re
import numpy as np
import math

import PyPDF2
import tabula
from pprint import pprint
import pandas as pd
import xml.etree.cElementTree as ET

# global
debugTrace =  False

def main(pgm, argv):
    pages      = None

    cmds =  { "xml": printXml, "print":  printTable }

    try:
        opts, args = getopt.getopt(argv, "p:", ['ppages='])
    except getopt.GetoptError:
        print(pgm, ' -p pages')
        sys.exit(2)

    for opt, arg in opts:
        if opt == '-h':
            print(pgm, ' -p pages')
            sys.exit()
        elif opt in ("-p", "--ppage"):
            pages = arg

    cmd =  args.pop(0)

    if cmd in cmds: 
        table =  filesToTable( args )
        cmds[ cmd ] ( table )
    else:
        print("Not such {} command exists.".format( cmd ) )

def is_nan(x):
    return (x is np.nan or x != x)


# cmds
def printFilesTable(files):
    table =  filesToTable( files )
    printTable( table )

def printFilesXml(files):
    table = filesToTable( files )
    printXml( table )
# cmds

def printXml(table): 
    tree =   tableToXml( table )
    tree.write( sys.stdout,
                encoding='unicode',
                xml_declaration=True,
                default_namespace=None,
                method="xml",
                # *,
                short_empty_elements=True)


def printTable(table,  step =  10):
    if step >  0:
        print(table)
        for r in range(0,  len(table),  step):
            debug("r =  {},  step =  {},  r +  step =  {}".format(r,  step,  r +  step))
            with pd.option_context('display.max_rows', 1000,
                                   'display.max_columns', 7,
                                   'display.max_colwidth',  200,
                                   'display.width',  2000):
                print(table[ r : r +  step ])
    else: 
        with pd.option_context('display.max_rows', 1000,
                               'display.max_columns', 7,
                               'display.max_colwidth',  200,
                               'display.width',  2000):
              print(table)
              

def filesToTable(files): 
    table =  extractTable( files )
    table =  santizeTable( table )
    table =  santizeTableCells( table )
    return table

def sanitizeXmlTag(tag):
    xmltransform =  str.maketrans(": ",  "xx")
    return tag.translate(xmltransform)

def tableToXml(table):
    account      = ET.Element("account")
    transactions = ET.SubElement(account, "transactions")
    for i, row in table.iterrows():
        transaction = ET.SubElement(transactions, "transaction")
        for col in table.columns:
            subEleCol      = ET.SubElement(transaction, sanitizeXmlTag( col ))
            subEleCol.text = str( row[ col ] )
    return ET.ElementTree(account)


def santizeRowCells(index,  table):
    for col in table.columns:
        if isinstance(table.loc[index, col],  str):
            table.loc[index, col] =  table.loc[index, col].strip()

def santizeTableCells(table): 
    for i, row in table.iterrows():
        santizeRowCells(i,  table)
    return table


def joinCell(val1, val2): 
    if is_nan(val2):
        return val1
    elif is_nan(val1):
        return val2
    elif isinstance(val1, type(val2)) or isinstance(val2, type(val1)): 
        if isinstance(val2,  str):
            return val1 +  " " +  val2
        elif isinstance(val2,  (int, float)):
            return val1 +  val2
        else:
            raise NameError("Type error",  val1,  val2)
    else:
        raise NameError("Type error",  val1,  val2)

def joinPandaRows(updateIndx,  copyIndx,  table):
    default_col =  "Transaction Details"

    for col in table.columns:
        debug("table.loc[copyIndx, col] ",  table.loc[copyIndx, col] )
        if is_nan( table.loc[updateIndx, col] ):
            if default_col in table.columns:
                table.loc[updateIndx, default_col] = joinCell(table.loc[updateIndx, default_col],
                                                              table.loc[copyIndx, col])
            else:
                warning("No {} exists in {}".  format(default_col,  table.columns))
        elif not( is_nan( table.loc[copyIndx, col] ) ):
            table.loc[updateIndx, col]         = joinCell(table.loc[updateIndx, col],
                                                          table.loc[copyIndx, col])
    
def santizeTable(table):
    row_iterator = table.iterrows()
    lastIndx, last = next( row_iterator )  # take first item from row_iterator
    for i, row in row_iterator:
        # table.drop(i, inplace=True)
        debug("row ",  i,  row.values.tolist())
        if is_nan(row[ "Date" ]):
            joinPandaRows(lastIndx,  i,  table)
            table.drop(i, inplace=True)
        else:
            lastIndx = i
            last     =  row
    table.reset_index(drop=True, inplace=True)
    return table

def santizeTableReadOnly(table):
    1
    # senitizedTable =  pd.DataFrame( table.columns )
    # for i,  row in table.iterrows():
    #     if row[ "Date" ] is not np.nan: 
    #         print("row[Date] is not None ",  row["Date"],  type( row["Date"] ))
    # # senitizedTable



def extractTable(files): 
    Table =  pd.DataFrame()
    for file in files:
        if os.path.exists( file ): 
            fileTable = extractTableInFile( file )
            Table     = pd.concat([Table,  fileTable], sort=False, ignore_index=True)
            Table.reset_index(drop=True)
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
        fileTable = pd.concat([fileTable, pgTable], sort=False, ignore_index=True)
        fileTable.reset_index(drop=True)
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

def verbose( *  args ): 
    print( *args )

def warning( *  args ): 
    print( *args )

def info( *  args ):
    print( *args )





if __name__ == "__main__":
   main(sys.argv[0],  sys.argv[1:])
