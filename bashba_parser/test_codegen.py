import os
import subprocess
from os.path import exists

print("-- Testing codegen --")
print("Rebuilding everything")
os.system("rm a.out")
os.system("rm bashba.native")
os.system("ocamlbuild -clean")
print("Building bashba...")
os.system("ocamlbuild -pkgs llvm bashba.native")
os.system("export PATH=/usr/local/opt/llvm/bin:$PATH")

for filename in os.listdir("./codegen_test_files"):
    out = subprocess.getoutput(
            "./bashba.native -l codegen_test_files/{} > a.out".format(filename))
    if("error" in out):
        print(out)
        print("Test case " + filename + " ❌ failed")
    else:
        print("Test case " + filename + " ✅ passed!")
