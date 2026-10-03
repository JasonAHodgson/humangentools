"""Add the Allen Ancient DNA Resource (AADR) to humangentools, and give every
population a `temporal` classification.

Source: data-raw/v66.p1_1240K.aadr.PUB.anno (AADR v66.0, 1240K release).

Design notes
------------
* `temporal` is "ancient" or "modern", from the .anno's date mean in BP: 0 means
  present-day. No AADR Group ID contains both, so the label is unambiguous per
  population. Every pre-existing humangentools population is "modern".
* AADR Genetic IDs carry a data-type suffix (.AG/.SG/.DG/.TW/...), so they never
  match an existing sample id directly, and stripping the suffix makes one
  individual with two libraries collide. So:
      id         = the individual (suffix stripped) -- joins across datasets
      source_id  = the dataset's own key (for AADR, the full Genetic ID)
      data_type  = the AADR suffix, NA elsewhere
  The uniqueness invariant becomes (source_id, pop) rather than (id, pop).
* `region` is left NA for AADR: the .anno gives a Political Entity (a country),
  which is not the same thing as the region vocabulary used elsewhere. The
  country is recorded in population_desc instead.
* The D-PLACE link table is deliberately NOT extended here -- it is mid-review,
  and appending thousands of fuzzy AADR candidates would swamp it.
"""
import csv, collections, re, statistics

EX="inst/extdata/"
ANNO="data-raw/v66.p1_1240K.aadr.PUB.anno"
AADR_REF="Mallick et al. 2024. The Allen Ancient DNA Resource (AADR): a curated compendium of ancient human genomes. Sci Data 11:182. 10.1038/s41597-024-03031-7"
SUF=r'\.(AG|SG|DG|TW|IM|BY|AA|WGC|EC|REF)$'

def rd(p): return list(csv.DictReader(open(EX+p),delimiter="\t"))
def wr(p,rows,cols):
    with open(EX+p,"w") as f:
        f.write("\t".join(cols)+"\n")
        for r in rows: f.write("\t".join(str(r.get(c,"NA")) if r.get(c,"") not in ("",None) else "NA" for c in cols)+"\n")

P=rd("population_information.Rtable"); S=rd("sample_information.Rtable")
D=rd("dataset_information.Rtable");    A=rd("population_assignment.Rtable")

# ---- read the .anno -------------------------------------------------------
raw=csv.reader(open(ANNO,encoding="utf-8",errors="replace"),delimiter="\t"); next(raw)
def num(x):
    try: return float(x)
    except: return None
rec=[]
for r in raw:
    if not any(r): continue
    g=lambda i: r[i] if i<len(r) else ""
    gen=g(0)
    m=re.search(SUF,gen)
    rec.append(dict(source_id=gen, id=re.sub(SUF,'',gen), data_type=(m.group(1) if m else "NA"),
                    grp=g(14), country=g(16), lat=num(g(17)), lon=num(g(18)), date=num(g(10))))
print(f"read {len(rec)} AADR individuals")

# ---- new columns on the existing tables ----------------------------------
Pcols=list(P[0]); Scols=list(S[0]); Dcols=list(D[0]); Acols=list(A[0])
def insert_after(cols,new,after):
    if new in cols: return cols
    c=list(cols); c.insert(c.index(after)+1,new); return c
Pcols=insert_after(Pcols,"temporal","source_dataset")
Scols=insert_after(Scols,"source_id","id")
Scols=insert_after(Scols,"data_type","source_id")
Scols=insert_after(Scols,"temporal","source_dataset")
for p in P: p["temporal"]="modern"
for s in S: s.setdefault("source_id",s["id"]); s["source_id"]=s["id"]; s["data_type"]="NA"; s["temporal"]="modern"

# ---- AADR populations ----------------------------------------------------
bygrp=collections.defaultdict(list)
for x in rec: bygrp[x["grp"]].append(x)
coord_varies=0; newP=[]
for grp,mem in sorted(bygrp.items()):
    temporal="modern" if all(m["date"] is not None and m["date"]<=0 for m in mem) else "ancient"
    lats=[m["lat"] for m in mem if m["lat"] is not None]
    lons=[m["lon"] for m in mem if m["lon"] is not None]
    if lats and (max(lats)-min(lats)>0.5 or max(lons)-min(lons)>0.5): coord_varies+=1
    lat=f"{statistics.median(lats):.6g}" if lats else "NA"
    lon=f"{statistics.median(lons):.6g}" if lons else "NA"
    countries=sorted({m["country"] for m in mem if m["country"]})
    desc=f"{grp} ({'; '.join(countries[:3])})" if countries else grp
    note=("archaeological find location (median over individuals in the group)"
          if temporal=="ancient" else
          "population location from the AADR .anno (median over individuals in the group)")
    row={c:"NA" for c in Pcols}
    row.update(pop=grp+"AADR", population_label=grp, population_desc=desc,
               source_dataset="AADR", temporal=temporal,
               origin_lat=lat, origin_lon=lon, sampling_lat=lat, sampling_lon=lon,
               coord_note=note, n_samples=len(mem), reference=AADR_REF)
    newP.append(row)
print(f"AADR populations: {len(newP)}  (groups whose member coordinates span >0.5 deg: {coord_varies})")

# ---- AADR samples --------------------------------------------------------
newS=[]
for x in rec:
    newS.append({ "id":x["id"], "source_id":x["source_id"], "data_type":x["data_type"],
                  "pop":x["grp"]+"AADR", "population":x["grp"], "region":"NA",
                  "source_dataset":"AADR",
                  "temporal":"modern" if all(m["date"] is not None and m["date"]<=0 for m in bygrp[x["grp"]]) else "ancient" })

P=P+newP; S=S+newS

# ---- dataset_information -------------------------------------------------
if not any(d["dataset"]=="AADR" for d in D):
    D.append({c:"NA" for c in Dcols} | dict(dataset="AADR",
        description="Allen Ancient DNA Resource v66.0, 1240K release (ancient and present-day individuals)",
        genotyping="1240K capture, shotgun, and Human Origins array (see data_type)",
        reference=AADR_REF))
for d in D:
    d["n_pops"]=len({r["pop"] for r in S if r["source_dataset"]==d["dataset"]})
    d["n_samples"]=sum(1 for r in S if r["source_dataset"]==d["dataset"])
D=[d for d in D if int(d["n_samples"])>0]; D.sort(key=lambda d:d["dataset"])

# ---- population_assignment ----------------------------------------------
for grp,mem in sorted(bygrp.items()):
    dts=sorted({m["data_type"] for m in mem})
    A.append({c:"NA" for c in Acols} | dict(population_label=grp,
        id_class="AADR_ID("+",".join(dts)+")", n_samples=len(mem), source_dataset="AADR",
        pop=grp+"AADR", assignment_basis="AADR .anno Group ID", hgt_dataset_old="NA",
        reviewed="FALSE"))

wr("population_information.Rtable",P,Pcols)
wr("sample_information.Rtable",S,Scols)
wr("dataset_information.Rtable",D,Dcols)
wr("population_assignment.Rtable",A,Acols)
print(f"\npopulations: {len(P)}   samples: {len(S)}")
for d in D: print(f"   {d['dataset']:24} pops={d['n_pops']:5} samples={d['n_samples']:6}")
t=collections.Counter(p["temporal"] for p in P)
print("\npopulations by temporal:",dict(t))
print("samples by temporal:",dict(collections.Counter(r["temporal"] for r in S)))
