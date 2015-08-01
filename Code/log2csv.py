# -*- coding: utf-8 -*-
"""
Spyder Editor
Transforming log file into a csv
"""

import json
import csv
import sys
import pandas as pd
import os

from collections import OrderedDict
def flatten(d):
    "Flatten an OrderedDict object"
    result = OrderedDict()
    for k, v in d.items():
        if isinstance(v, dict):
            result.update(flatten(v))
        else:
            result[k] = v
    return result

def log2csv(filename):
    raw_log_data = [json.loads(i) for i in open(filename)]
    log_data = pd.DataFrame.from_dict([flatten(i) for i in raw_log_data])
    log_data.to_csv(filename[:-4] + '.csv', index=False)
    
log2csv('/Users/Vamsi/myflaskapp/app-root/logs/cuing_webapp.log')