"""
Rebuild humangentools population/sample/dataset tables on canonical kgp::allmeta codes.

Run normalise_population_labels.py at the end of the chain (after add_aadr.py
and apply_review.py): it reduces each population to a single population_label
and moves the alternative names to population_alt.

All judgements are explicit in the maps below. Anything not resolvable is marked
source_dataset = "unconfirmed" rather than guessed.
"""
import csv, pyreadr, collections, re, os
U="/mnt/user-data/uploads/humangentools"; OUT="build/out"; os.makedirs(OUT,exist_ok=True)
AM=pyreadr.read_r("kgp/R/sysdata.rda")["allmeta"]; KGPE=pyreadr.read_r("kgp/R/sysdata.rda")["kgpe"]
kid={r.id:r.pop for r in KGPE.itertuples()}
amby={r.pop:r for r in AM.itertuples()}
def nm(s): return re.sub(r'[^a-z]','',str(s).lower())
amsgdp={nm(p.replace("SGDP","")):p for p in amby if p.endswith("SGDP")}

# ---- explicit HGDP label -> allmeta hgdp code -------------------------------
HGDP_MAP={"Adygei":"Adygei","Balochi":"Balochi","Bantu_North":"BantuKenya",
 "Bantu_South":"BantuSouthAfrica","Basque":"Basque","Bedouin":"Bedouin",
 "Bergamo":"BergamoItalian","Biaka_Pygmy":"Biaka","Brahui":"Brahui","Burusho":"Burusho",
 "Cambodian":"Cambodian","Dai":"Dai","Daur":"Daur","Druze":"Druze","French":"French",
 "Hazara":"Hazara","Hezhen":"Hezhen","Japanese":"Japanese","Kalash":"Kalash",
 "Karitiana":"Karitiana","Lahu":"Lahu","Makrani":"Makrani","Mandenka":"Mandenka",
 "Maya":"Maya","Mbuti_Pygmy":"Mbuti","Melanesian":"Bougainville","Miaozu":"Miao",
 "Mongolia":"Mongolian","Mozabite":"Mozabite","Naxi":"Naxi","Orcadian":"Orcadian",
 "Oroqen":"Oroqen","Palestinian":"Palestinian","Pathan":"Pathan",
 "Piapoco_and_Curripaco":"Colombian",   # HGDP's "Colombian" panel IS Piapoco + Curripaco
 "Pima":"Pima","Russia":"Russian","San":"San","Sardinia":"Sardinian","She":"She",
 "Sindhi":"Sindhi","Surui":"Surui","Tu":"Tu","Tujia":"Tujia","Tuscan":"Tuscan",
 "Uygur":"Uygur","Xibo":"Xibo","Yakut":"Yakut","Yizu":"Yi","Yoruba":"Yoruba"}
# HGDP labels that span TWO allmeta populations -> cannot be assigned one code
HGDP_SPLIT={"Han":"HanHGDP + NorthernHanHGDP","Papuan":"PapuanHighlandsHGDP + PapuanSepikHGDP"}
# ---- HapMap: label -> HapMap population code
HAPMAP={"African_American":"ASW","Han_Chinese":"CHB","Chinese_Denver":"CHD","Gujarati":"GIH",
 "Japanese":"JPT","Luhya":"LWK","Mexican_American":"MEX","Mexican_LA":"MEX","Maasai":"MKK",
 "Tuscan":"TSI","Yoruba":"YRI","CEPH_Europeans":"CEU","China_Japan":"JPTCHB","European":"CEU"}
RAK={"Merina","Betsileo","Betsimisaraka","Sakalava_Tsimihety","Vezo","Mikea","Temoro","Diego"}
PERRY={"Batwa","Kiga"}
# KGP populations sampled outside the group's homeland -> origin is not a point
KGP_DIASPORA={"ASW":"African ancestry in the southwestern US; origin is a distribution over West/Central African source populations, not a point",
 "ACB":"African Caribbean in Barbados; origin is a distribution over West/Central African source populations, not a point",
 "CEU":"Utah residents of northern and western European ancestry; origin diffuse across NW Europe",
 "MXL":"Mexican ancestry in Los Angeles; origin is a distribution within Mexico, not a point"}
# KGP diaspora populations whose homeland IS unambiguous (named in the population description)
KGP_ORIGIN={"GIH":(22.5,71.5,"Gujarat, India (sampled in Houston)"),
 "ITU":(16.5,79.5,"Telangana/Andhra Pradesh, India (sampled in the UK)"),
 "STU":(7.5,80.8,"Sri Lanka (sampled in the UK)")}

def idclass(i):
    if i in kid: return "KGP"
    if i.startswith("HGDP"): return "HGDP_ID"
    if re.match(r'^NA\d',i): return "NA_ID"
    if re.match(r'^HG\d',i): return "HG_ID"
    return "OTHER_ID"

hs=list(csv.DictReader(open(f"{U}/inst/extdata/sample_information.Rtable"),delimiter="\t"))
hp={r["population"]:r for r in csv.DictReader(open(f"{U}/inst/extdata/population_information.Rtable"),delimiter="\t")}

def resolve(label,cls):
    if cls=="KGP":
        codes={kid[s["id"]] for s in hs if s["population"]==label and s["id"] in kid}
        return ("KGP",codes.pop(),"sample membership in kgp::kgpe") if len(codes)==1 \
               else ("KGP","AMBIGUOUS","several kgp codes under one label")
    if label in RAK:   return "Rakotoarivony_etal",label+"RAK","stated by data owner"
    if label in PERRY: return "Perry_etal_unconfirmed",label+"PER","provisional; provenance to confirm"
    if cls=="NA_ID" and label in HAPMAP:
        return "HapMap",HAPMAP[label]+"HapMap","HapMap-era Coriell IDs under a HapMap population"
    if cls=="HGDP_ID":
        if label in HGDP_SPLIT: return "HGDP","NEEDS_SPLIT","label spans "+HGDP_SPLIT[label]
        if label in HGDP_MAP:   return "HGDP",HGDP_MAP[label]+"HGDP","kgp::allmeta hgdp"
    SGDP_ALIAS={"masai":"MKK"}          # SGDP codes its Maasai samples MKK
    key=nm(label.replace("_"," "))
    key=SGDP_ALIAS.get(key,key).lower() if key in SGDP_ALIAS else key
    if key in amsgdp: return "SGDP",amsgdp[key],"kgp::allmeta sgdp"
    if nm(label) in {"masai"}: return "SGDP","MKKSGDP","kgp::allmeta sgdp (SGDP codes Maasai as MKK)"
    return "unconfirmed",label+"UNK","no kgp::allmeta entry and provenance not recoverable from the file"

groups=collections.Counter((s["population"],idclass(s["id"])) for s in hs)
asg={}; arows=[]
for (label,cls),n in sorted(groups.items()):
    ds,code,basis=resolve(label,cls); asg[(label,cls)]=(ds,code)
    arows.append(dict(population_label=label,id_class=cls,n_samples=n,source_dataset=ds,pop=code,
                      assignment_basis=basis,hgt_dataset_old=hp.get(label,{}).get("dataset","NA"),
                      reviewed="FALSE"))
with open(f"{OUT}/population_assignment.Rtable","w",newline="") as f:
    w=csv.DictWriter(f,fieldnames=list(arows[0]),delimiter="\t"); w.writeheader(); w.writerows(arows)

# ---- sample_information ----------------------------------------------------
srows=[]
for s in hs:
    cls=idclass(s["id"]); ds,code=asg[(s["population"],cls)]
    srows.append(dict(id=s["id"],pop=code,population=s["population"],
                      region=s["region"],source_dataset=ds))
# two source labels can map to the same canonical code (e.g. Mexican_American and
# Mexican_LA both -> MXL); after remapping those are true duplicate rows, so
# collapse them. The surviving row keeps the label it already had: joining the
# two labels here would write a combined label onto just the samples that
# arrived under both names, splitting the population when samples are counted.
# The alternative names are recovered per population below, not per sample, and
# normalise_population_labels.py puts them in population_alt.
bykey=collections.OrderedDict()
alt_labels=collections.defaultdict(set)   # pop -> every source label seen
for r in srows:
    k=(r["id"],r["pop"])
    if k in bykey:
        alt_labels[r["pop"]].add(r["population"])
        alt_labels[r["pop"]].add(bykey[k]["population"])
    else: bykey[k]=r
n_collapsed=len(srows)-len(bykey)
srows=list(bykey.values())
print(f"collapsed {n_collapsed} rows that became duplicates under the canonical code")
srows.sort(key=lambda r:(r["source_dataset"],r["pop"],r["id"]))
with open(f"{OUT}/sample_information.Rtable","w",newline="") as f:
    w=csv.DictWriter(f,fieldnames=["id","pop","population","region","source_dataset"],delimiter="\t")
    w.writeheader(); w.writerows(srows)

# ---- population_information ------------------------------------------------
bypop=collections.defaultdict(list)
for r in srows: bypop[(r["pop"],r["source_dataset"])].append(r)
prows=[]
for (code,ds),mem in sorted(bypop.items()):
    labels=sorted({m["population"] for m in mem} | alt_labels.get(code,set()))
    a=amby.get(code)
    desc=str(a.population) if a is not None else "NA"
    reg_kgp=str(a.region).replace(" ","_") if a is not None else "NA"
    old=hp.get(labels[0],{})
    region = reg_kgp if (ds=="KGP" and a is not None) else old.get("region","NA")
    olat=olon=slat=slon="NA"; note="NA"
    if a is None and ds=="HapMap":
        kcode={"MEX":"MXL"}.get(code.replace("HapMap",""),code.replace("HapMap",""))
        ka=amby.get(kcode)
        if ka is not None:
            slat,slon=f"{float(ka.lat):.6g}",f"{float(ka.lng):.6g}"
            note=("sampling location inherited from the same-named 1000 Genomes population "
                  f"({kcode}); HapMap collection site assumed identical")
            desc=f"{str(ka.population)} (HapMap)"
            reg_kgp=str(ka.region).replace(" ","_")
            if kcode not in KGP_DIASPORA: olat,olon=slat,slon
    if a is not None:
        if ds=="KGP":
            slat,slon=f"{float(a.lat):.6g}",f"{float(a.lng):.6g}"
            if code in KGP_ORIGIN:
                olat,olon,note=KGP_ORIGIN[code][0],KGP_ORIGIN[code][1],KGP_ORIGIN[code][2]
            elif code in KGP_DIASPORA: note=KGP_DIASPORA[code]
            else: olat,olon,note=slat,slon,"sampled within the group's homeland"
        else:
            olat,olon=f"{float(a.lat):.6g}",f"{float(a.lng):.6g}"
            slat,slon=olat,olon
            note="population location from kgp::allmeta; collection in situ, exact site not recorded"
    prows.append(dict(pop=code,population_label="|".join(labels),population_desc=desc,
        source_dataset=ds,region=region,region_kgp=reg_kgp,
        origin_lat=olat,origin_lon=olon,sampling_lat=slat,sampling_lon=slon,
        coord_note=note,n_samples=len(mem),reference=old.get("reference","NA")))
with open(f"{OUT}/population_information.Rtable","w",newline="") as f:
    w=csv.DictWriter(f,fieldnames=list(prows[0]),delimiter="\t",quoting=csv.QUOTE_MINIMAL)
    w.writeheader(); w.writerows(prows)

# ---- dataset_information --------------------------------------------------
DS={"KGP":("1000 Genomes Project (phase 3 + NYGC high-coverage expansion)","WGS","Byrska-Bishop et al. 2022. Cell 185(18) 3426-3440. 10.1016/j.cell.2022.08.004"),
 "HGDP":("Human Genome Diversity Project cell line panel","SNP array / WGS","Cann et al. 2002. Science 296(5566) 261-262. 10.1126/science.296.5566.261b"),
 "SGDP":("Simons Genome Diversity Project","WGS","Mallick et al. 2016. Nature 538(7624) 201-206. 10.1038/nature18964"),
 "HapMap":("International HapMap Project (phase 3 and HapMap-era Coriell panels)","SNP array","International HapMap 3 Consortium 2010. Nature 467(7311) 52-58. 10.1038/nature09298"),
 "Rakotoarivony_etal":("Madagascar population genomics","SNP array","Rakotoarivony et al. bioRxiv preprint"),
 "Perry_etal_unconfirmed":("Batwa / Kiga comparative dataset; provenance provisional","NA","to confirm"),
 "unconfirmed":("Samples whose source dataset is not recorded in the package data","NA","NA")}
drows=[]
for ds in sorted({r["source_dataset"] for r in srows}):
    d=DS.get(ds,("NA","NA","NA"))
    drows.append(dict(dataset=ds,description=d[0],genotyping=d[1],
        n_pops=len({r['pop'] for r in srows if r['source_dataset']==ds}),
        n_samples=sum(1 for r in srows if r["source_dataset"]==ds),reference=d[2]))
with open(f"{OUT}/dataset_information.Rtable","w",newline="") as f:
    w=csv.DictWriter(f,fieldnames=list(drows[0]),delimiter="\t",quoting=csv.QUOTE_MINIMAL)
    w.writeheader(); w.writerows(drows)

print(f"populations: {len(prows)}   samples: {len(srows)}   datasets: {len(drows)}\n")
for d in drows: print(f"  {d['dataset']:24} pops={d['n_pops']:4} samples={d['n_samples']:5}  {d['genotyping']}")
print("\nunresolved / needing review:")
for r in arows:
    if r["source_dataset"]=="unconfirmed" or r["pop"] in ("AMBIGUOUS","NEEDS_SPLIT"):
        print(f"  {r['population_label']:24}{r['id_class']:9}n={r['n_samples']:<5}{r['pop']:28}{r['assignment_basis'][:52]}")
