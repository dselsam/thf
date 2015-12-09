import os
import subprocess

THF_PATH = "/home/dselsam/github/thf/"

src_prefix = THF_PATH + "isa_filter_v2/problems/"
dst_prefix = THF_PATH + "sledgehammer_lean/"

f = THF_PATH + "dist/build/thf/thf"

subprocess.call(["rm","-rf",dst_prefix])
subprocess.call(["mkdir",dst_prefix])
subprocess.call(["mkdir",dst_prefix + "incl/"])

for filename in os.listdir(src_prefix):
  if filename.endswith(".p"):
    new_filename = filename.split(".")[0] + ".lean"
    out_file = open(dst_prefix + new_filename, "w")
    proc = subprocess.Popen([f, src_prefix + filename], stdout=out_file)
#    proc.wait() # Note: this slows the script down tremendously and does not seem to be necessary

for filename in os.listdir(src_prefix + "incl"):
  if filename.endswith(".ax"):
    new_filename = filename.split(".")[0] + ".lean"
    out_file = open(dst_prefix + "incl/" + new_filename, "w")
    if filename != "prelude.ax": out_file.write("import incl.prelude\n")
    proc = subprocess.Popen([f, src_prefix + "incl/" + filename], stdout=out_file)
#    proc.wait()

project_file = open(dst_prefix + ".project","w")
project_file.write("+ *.lean\n")
project_file.write("- flycheck*.lean\n")
project_file.write("- .#*.lean\n")
