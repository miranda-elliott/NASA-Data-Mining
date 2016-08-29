# -*- coding: utf-8 -*-
"""
data mining from txt to csv
for NASA SRD & ERD documents

file: txt_to_csv_miner.py
author: Miranda Elliott
date: 04/19/2016
"""

import io
import sys
import csv
import re

# document configuration fields
filename_in = str(input('Enter .txt filename: ')).strip()
filename_out = str(input('Enter .csv filename: ')).strip()
keyword = str(input('Enter document keyword: ')).strip()

# flag term lists
flag_num = ['+/-', '%', ' days', ' dB', ' degrees', ' feet', ' ft', ' hours', ' Hz', ' in', ' inches', ' lbf', ' lbm', ' MHz', ' milliseconds', ' ms', ' percent', ' pounds', ' psf', ' second', ' seconds', ' slug']
flag_txt = ['avionics', 'roll', 'yaw', 'pitch']

# column names
COL_REQ_ID = 'Requirement ID'
COL_REQ_TITLE = 'Requirement Title'
COL_REQ_TEXT = 'Requirement Text'
COL_RAT = 'Rationale'
COL_TRACE = 'Trace'
COL_ALLOC = 'Allocation'
COL_PRIOR = 'Priority'
COL_VERIF = 'Verification'
COL_FLAG_NUM = 'Numeric in "Requirement Text" field'
COL_FLAG_TXT = 'Keyword in "Rationale" or "Requirement Text" fields'

# open txt file to read
try:
    f_in = io.open(filename_in, 'r', encoding='utf-8')
    document = f_in.read().split('\n')
except IOError:
    sys.exit('txt file not found in directory. please check its location and spelling and try again.')

# open csv file to read 
f_out = io.open(filename_out, 'w')
writer = csv.writer(f_out)
header = False

# iterate over each line in txt file
for i in range(0, len(document)):
    # if line matches requirement format, mine for text
    if keyword in document[i]:
        if ((COL_RAT in document[i+2]) & (COL_TRACE in document[i+3]) & (COL_ALLOC in document[i+4])):
            
            # write column header row to csv (executed once)
            if (header == False):
                if (COL_PRIOR in document[i+5]):
                    writer.writerow([COL_REQ_ID, COL_REQ_TITLE, COL_REQ_TEXT, COL_RAT, COL_TRACE, COL_ALLOC, COL_PRIOR, COL_FLAG_NUM, COL_FLAG_TXT])
                if (COL_VERIF in document[i+5]):
                    writer.writerow([COL_REQ_ID, COL_REQ_TITLE, COL_REQ_TEXT, COL_RAT, COL_TRACE, COL_ALLOC, COL_VERIF, COL_FLAG_NUM, COL_FLAG_TXT])
                header = True
            
            # construct data row
            ### mine requirement fields ###  
            # req id
            data1 = ''.join([char if ord(char) < 128 else '' for char in document[i]]).strip()
            id_regex = keyword + '-?\d+'
            col1 = re.findall(id_regex, data1)[0]
                
            # req title
            data2 = data1.split()
            if (col1 in data2[0]):
                col2 = ' '.join(data2[1:len(data2)])
            elif (col1 in data2[1]):
                col2 = ' '.join(data2[2:len(data2)])
            
            # req text
            col3 = ''.join([char if ord(char) < 128 else '' for char in document[i+1]]).strip()
            
            # rationale
            data4 = ''.join([char if ord(char) < 128 else '' for char in document[i+2]]).strip().strip('[').strip(']').split()
            col4 = ' '.join(data4[1:len(data4)])

            # trace 
            data5 = ''.join([char if ord(char) < 128 else '' for char in document[i+3]]).strip().strip('[').strip(']').split()
            col5 = ' '.join(data5[1:len(data5)])
            
            # allocation 
            data6 = ''.join([char if ord(char) < 128 else '' for char in document[i+4]]).strip().strip('[').strip(']').split()
            col6 = ' '.join(data6[1:len(data6)])
            
            # priority/verification
            if ((COL_PRIOR in document[i+5]) | (COL_VERIF in document[i+5])):
                data7 = ''.join([char if ord(char) < 128 else '' for char in document[i+5]]).strip().strip('[').strip(']').split()
                col7 = ' '.join(data7[1:len(data7)])
            else:
                col7 = ''
            
            ### flag requirement fields ###
            # flag for numeric measurement in req text
            col8 = any(char.isnumeric() for char in str(col3))
            num_count = 0
            if col8:
                for m in flag_num:
                    if m in col3:
                        num_count += 1
                if num_count == 0: 
                    col8 = False
                    
            # flag for word in req text or rationale
            col9 = False
            word_count = 0
            for w in flag_txt:
                if ((w.lower() in col3.lower()) | (w.lower() in col4.lower())):
                    word_count += 1
            if word_count != 0:
                col9 = True
            
            # write data row to csv 
            writer.writerow([col1, col2, col3, col4, col5, col6, col7, col8, col9])
            
# close files 
print('wrote file', filename_out)
f_in.close()
f_out.close()
