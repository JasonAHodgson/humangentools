"""Apply the reviewed verdicts from the xlsx back into population_dplace_link.Rtable.

accept  + soc_id   -> keep, reviewed = TRUE
accept, no soc_id  -> keep with soc_id NA, reviewed = TRUE  (a confirmed dead end)
reject             -> drop the row
replace            -> soc_id := corrected_soc_id, society fields re-looked-up from
                      dplaceR, match_method = "manual", confidence = "high",
                      match_score / geo_distance_km = NA (matcher outputs are
                      meaningless for a hand match), marriage count recomputed
"""
import csv, collections, pyreadr
from openpyxl import load_workbook

XLSX="/mnt/user-data/uploads/humangentools/data-raw/population_dplace_link_review.xlsx"
SRC="/mnt/user-data/uploads/humangentools/inst/extdata/population_dplace_link.Rtable"
OUT="/mnt/user-data/outputs/population_dplace_link.Rtable"

soc=pyreadr.read_r("dplaceR/data/dplace_societies.rda")["dplace_societies"]
SOC={r.soc_id:r for r in soc.itertuples()}
vals=pyreadr.read_r("dplaceR/data/dplace_values.rda")["dplace_values"]
vals=vals[~(vals.code_id.notna() & vals.code_id.astype(str).str.endswith("-NA"))]
MARRIAGE=["EA009","EA012","EA015","EA018","EA020","EA023","EA024","EA025","EA026","EA043"]
coded=collections.defaultdict(set)
for v,s in zip(vals.var_id,vals.soc_id): coded[v].add(s)
nmar=lambda s: str(sum(1 for v in MARRIAGE if s in coded[v])) if s and s!="NA" else "NA"

ws=load_workbook(XLSX)["review"]
hdr=[c.value for c in ws[1]]
rev=[{h:ws.cell(row=i,column=c).value for c,h in enumerate(hdr,start=1)} for i in range(2,ws.max_row+1)]
rev=[r for r in rev if r.get("pop")]

cols=list(csv.DictReader(open(SRC),delimiter="\t").fieldnames)
if "review_note" not in cols: cols.append("review_note")
base={(r["pop"],r["soc_id"],r["match_method"]):r for r in csv.DictReader(open(SRC),delimiter="\t")}
S=lambda x: (str(x).strip() if x is not None else "")

out=[]; dropped=0; replaced=[]; confirmed_none=0; unknown=[]
for r in rev:
    verdict=S(r["verdict"]).lower()
    key=(r["pop"], S(r["soc_id"]) or "NA", S(r["match_method"]) or "NA")
    row=dict(base.get(key, {}))
    if not row:
        unknown.append(key); continue
    row.setdefault("review_note","NA")
    if S(r["notes"]): row["review_note"]=S(r["notes"])
    if verdict=="reject":
        dropped+=1; continue
    if verdict=="replace":
        new=S(r["corrected_soc_id"])
        if not new: unknown.append(("replace with no corrected_soc_id",)+key); continue
        s=SOC.get(new)
        row=dict(row)
        row["soc_id"]=new
        row["society_name"]=str(s.name) if s is not None else "NA"
        reg=str(s.region) if s is not None else "NA"
        row["society_region"]= "NA" if reg in ("nan","None","") else reg
        row["match_method"]="manual"; row["confidence"]="high"
        row["match_score"]="NA"; row["geo_distance_km"]="NA"
        row["n_marriage_vars_coded"]=nmar(new)
        row["reviewed"]="TRUE"
        replaced.append((r["pop"],new,row["society_name"],row["n_marriage_vars_coded"]))
        out.append(row); continue
    if verdict=="accept":
        row["reviewed"]="TRUE"
        if not S(r["soc_id"]): confirmed_none+=1
        out.append(row); continue
    unknown.append(("unrecognised verdict "+repr(r["verdict"]),)+key)

# a replace can produce two rows from one original key (two societies for one pop)
seen=set(); ded=[]
for r in out:
    k=(r["pop"],r["soc_id"],r["match_method"])
    if k in seen: continue
    seen.add(k); ded.append(r)
ded.sort(key=lambda r:(r["source_dataset"],r["pop"],r["confidence"]))
with open(OUT,"w") as f:
    f.write("\t".join(cols)+"\n")
    for r in ded: f.write("\t".join(str(r.get(c,"NA")) if r.get(c,"") not in ("",None) else "NA" for c in cols)+"\n")

print(f"reviewed rows read: {len(rev)}")
print(f"  rejected and dropped: {dropped}")
print(f"  accepted: {len(out)-len(replaced)}  (of which confirmed-no-match: {confirmed_none})")
print(f"  replaced: {len(replaced)}")
for p in replaced: print(f"      {p[0]:22} -> {p[1]:14} {p[2]:22} marriage vars {p[3]}")
print(f"  unresolved/ignored: {len(unknown)} {unknown[:3]}")
print(f"\nfinal link table: {len(ded)} rows (deduplicated from {len(out)})")
real=[r for r in ded if r["soc_id"]!="NA"]
print(f"  rows with a society: {len(real)}  distinct pops {len({r['pop'] for r in real})}  distinct societies {len({r['soc_id'] for r in real})}")
good=[r for r in real if r["n_marriage_vars_coded"] not in ("NA","") and int(r["n_marriage_vars_coded"])>=8]
print(f"  with >=8/10 marriage variables: {len(good)} rows, {len({r['pop'] for r in good})} pops, {len({r['soc_id'] for r in good})} societies")
print(f"  all rows reviewed: {all(r['reviewed']=='TRUE' for r in ded)}")
