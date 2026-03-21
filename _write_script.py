import os

script = open(os.path.expanduser(
    "~/ShinyApps/SOMEC_Editeur/interactive_mission_qc_console.R"
)).read()

# Strip everything before the first "# ====" line
lines = script.splitlines()
start = next((i for i, l in enumerate(lines) if l.startswith("# =====")), 0)
clean = "\n".join(lines[start:])

with open(os.path.expanduser(
    "~/ShinyApps/SOMEC_Editeur/interactive_mission_qc_console.R"
), "w", encoding="utf-8") as f:
    f.write(clean)

print(f"Done. First line: {clean.splitlines()[0]}")
print(f"Total lines: {len(clean.splitlines())}")
