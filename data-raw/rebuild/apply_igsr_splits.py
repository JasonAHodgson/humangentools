"""Apply IGSR-derived refinements to the installed humangentools tables.

1. Split the HGDP `Han` label into HanHGDP / NorthernHanHGDP and `Papuan` into
   PapuanSepikHGDP / PapuanHighlandsHGDP, per IGSR's per-sample
   `Population elastic ID` (data-raw/igsr_samples.tsv).
2. Reassign the `unconfirmed` samples that IGSR identifies as 1000 Genomes
   populations outside the NYGC 3,202 high-coverage release to a distinct
   source dataset, KGP_IGSR, with codes <CODE>IGSR. They are kept separate from
   KGP deliberately: pooling them would mix sequencing releases under one code,
   which is the conflation this schema exists to prevent.

Samples IGSR does not cover keep an explicit <base>unresolved code rather than
being folded silently into one side of a split.
"""
import csv, collections, os

EX="inst/extdata/"
ig={r["Sample name"]:r for r in csv.DictReader(open("data-raw/igsr_samples.tsv"),delimiter="\t")}
am={r["pop"]:r for r in csv.DictReader(open("perl/data/allmeta.tsv"),delimiter="\t")}

def read(p): return list(csv.DictReader(open(EX+p),delimiter="\t"))
def write(p,rows,cols):
    with open(EX+p,"w") as f:
        f.write("\t".join(cols)+"\n")
        for r in rows: f.write("\t".join(str(r.get(c,"NA")) for c in cols)+"\n")

S=read("sample_information.Rtable")
P=read("population_information.Rtable")
A=read("population_assignment.Rtable")
D=read("dataset_information.Rtable")
L=read("population_dplace_link.Rtable")
Pcols=list(P[0]); Scols=list(S[0]); Acols=list(A[0]); Dcols=list(D[0]); Lcols=list(L[0])

def elastic(sid, suffix):
    return [x for x in ig.get(sid,{}).get("Population elastic ID","").split(",")
            if x and x.endswith(suffix)]

# ---- 1. HGDP splits --------------------------------------------------------
SPLITS={"Han":"HGDP","Papuan":"HGDP"}
moved=collections.Counter()
for r in S:
    if r["population"] in SPLITS and r["source_dataset"]=="HGDP":
        e=elastic(r["id"],"HGDP")
        new=e[0] if e else r["population"]+"HGDPunresolved"
        if new!=r["pop"]: moved[(r["pop"],new)]+=1
        r["pop"]=new

# ---- 2. IGSR-only 1000 Genomes samples ------------------------------------
igsr_moved=collections.Counter()
for r in S:
    if r["source_dataset"]!="unconfirmed": continue
    g=ig.get(r["id"])
    if not g: continue
    code=g["Population code"].strip()
    if not code: continue
    new=code+"IGSR"
    igsr_moved[(r["pop"],new)]+=1
    r["pop"]=new; r["source_dataset"]="KGP_IGSR"

# ---- rebuild population_information for every code now in use -------------
bypop=collections.defaultdict(list)
for r in S: bypop[(r["pop"],r["source_dataset"])].append(r)
old={ (p["pop"],p["source_dataset"]):p for p in P }
KGP_DIASPORA={"ASW","ACB","CEU","MXL"}
newP=[]
# population_label may come out pipe-joined here where one canonical code drew
# samples carrying different source labels. That is the full set of names for the
# population, which is what we want; normalise_population_labels.py, run at the
# end of the chain, picks the primary and moves the rest to population_alt.
for (code,ds),mem in sorted(bypop.items()):
    if (code,ds) in old:
        p=dict(old[(code,ds)]); p["n_samples"]=len(mem)
        p["population_label"]="|".join(sorted({l for m in mem for l in m["population"].split("|")}))
        newP.append(p); continue
    base=code[:-4] if ds=="KGP_IGSR" and code.endswith("IGSR") else code
    a=am.get(base)
    p={c:"NA" for c in Pcols}
    p.update(pop=code, source_dataset=ds, n_samples=len(mem),
             population_label="|".join(sorted({l for m in mem for l in m["population"].split("|")})))
    ref=next((o["reference"] for (c,d),o in old.items() if d==ds), "NA")
    p["reference"]=ref
    if a:
        p["population_desc"]=a["population"] + (" (IGSR, outside the NYGC 3,202 release)" if ds=="KGP_IGSR" else "")
        p["region_kgp"]=a["region"].replace(" ","_")
        p["region"]=p["region_kgp"]
        p["sampling_lat"],p["sampling_lon"]=a["lat"],a["lng"]
        if ds=="KGP_IGSR" and base in KGP_DIASPORA:
            p["coord_note"]=("sampled outside the group's homeland; origin is a distribution "
                             "over source populations, not a point")
        else:
            p["origin_lat"],p["origin_lon"]=a["lat"],a["lng"]
            p["coord_note"]=("population location from kgp::allmeta; collection in situ, "
                             "exact site not recorded") if ds!="KGP_IGSR" else \
                            "sampled within the group's homeland"
    else:
        p["population_desc"]="NA"
        p["coord_note"]=("samples IGSR does not cover, so the IGSR population split could "
                         "not be applied; reassign if a source is found")
    newP.append(p)

# ---- dataset_information ---------------------------------------------------
dsd={d["dataset"]:d for d in D}
if "KGP_IGSR" not in dsd:
    dsd["KGP_IGSR"]=dict(dataset="KGP_IGSR",
      description=("1000 Genomes samples registered in IGSR but outside the NYGC 3,202 "
                   "high-coverage release"),
      genotyping="varies by IGSR data collection",
      reference="IGSR; https://www.internationalgenome.org/")
for k,d in dsd.items():
    d["n_pops"]=len({r["pop"] for r in S if r["source_dataset"]==k})
    d["n_samples"]=sum(1 for r in S if r["source_dataset"]==k)
newD=[d for d in dsd.values() if d["n_samples"]>0]
newD.sort(key=lambda d:d["dataset"])

# ---- assignment table ------------------------------------------------------
pop_of=collections.defaultdict(set)
for r in S: pop_of[(r["population"],r["source_dataset"])].add(r["pop"])
for a in A:
    k=(a["population_label"],a["source_dataset"])
    if a["source_dataset"]=="unconfirmed":
        k2=(a["population_label"],"KGP_IGSR")
        if pop_of.get(k2):
            a["source_dataset"]="KGP_IGSR"; a["pop"]="|".join(sorted(pop_of[k2]))
            a["assignment_basis"]="IGSR per-sample Population code (outside NYGC 3,202)"
            continue
    if pop_of.get(k) and a["pop"] in ("NEEDS_SPLIT",):
        a["pop"]="|".join(sorted(pop_of[k]))
        a["assignment_basis"]="IGSR per-sample Population elastic ID"

# ---- link table: new codes inherit their label's society -------------------
bylabel=collections.defaultdict(list)
for l in L: bylabel[l["population_label"]].append(l)
have={(l["pop"],l["soc_id"],l["match_method"]) for l in L}
newL=list(L)
for p in newP:
    if any(l["pop"]==p["pop"] for l in L): continue
    for lab in p["population_label"].split("|"):
        for src in bylabel.get(lab,[]):
            k=(p["pop"],src["soc_id"],src["match_method"])
            if k in have: continue
            have.add(k)
            n=dict(src); n["pop"]=p["pop"]; n["source_dataset"]=p["source_dataset"]
            n["population_label"]=lab; newL.append(n)
newL.sort(key=lambda r:(r["source_dataset"],r["pop"],r["confidence"]))

write("sample_information.Rtable",S,Scols)
write("population_information.Rtable",newP,Pcols)
write("dataset_information.Rtable",newD,Dcols)
write("population_assignment.Rtable",A,Acols)
write("population_dplace_link.Rtable",newL,Lcols)

print("HGDP splits applied:")
for (o,n),c in sorted(moved.items()): print(f"    {o:16} -> {n:28} {c}")
print(f"\nIGSR-only 1000 Genomes samples retagged: {sum(igsr_moved.values())}")
for (o,n),c in sorted(igsr_moved.items()): print(f"    {o:22} -> {n:12} {c}")
print(f"\npopulations: {len(P)} -> {len(newP)}   link rows: {len(L)} -> {len(newL)}")
print("\ndatasets now:")
for d in newD: print(f"    {d['dataset']:24} pops={d['n_pops']:4} samples={d['n_samples']:5}")
