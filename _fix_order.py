with open("interactive_mission_qc_console.R", "r") as f:
    content = f.read()

SEP = "# " + "=" * 60 + "\n"

loop_marker = SEP + "# INTERACTIVE LOOP"
cv_marker   = SEP + "# CROSS-VALIDATION RULES"
run_marker  = SEP + "# RUN\n" + SEP + "\nrun_interactive_qc()"

loop_start = content.index(loop_marker)
cv_start   = content.index(cv_marker)
run_start  = content.index(run_marker)

before_loop = content[:loop_start]
loop_block  = content[loop_start:cv_start]
cv_block    = content[cv_start:run_start]
run_block   = content[run_start:]

new_content = before_loop + cv_block + loop_block + run_block

with open("interactive_mission_qc_console.R", "w") as f:
    f.write(new_content)

print("Done — CROSS-VALIDATION moved before INTERACTIVE LOOP")
