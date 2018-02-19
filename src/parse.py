import json

with open("../dat/meta.json", "r") as f:
    meta = json.load(f)

for k in meta.keys():
    print meta[k]
    print

meta['description']
