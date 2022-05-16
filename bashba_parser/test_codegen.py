import os
import subprocess
from os.path import exists

print("-- Testing codegen --")
print("Rebuilding everything")
os.system("rm a.out")
os.system("rm bashba.native")
print("Building bashba...")
os.system("ocamlbuild -pkgs llvm bashba.native")
os.system("export PATH=/usr/local/opt/llvm/bin:$PATH")

for filename in os.listdir("./codegen_test_files"):
    if("error" in subprocess.getoutput(
            "./bashba.native -l {} > a.out".format(filename))):
        print("Test case " + filename + " ❌ failed")
    else:
        print("Test case " + filename + " ✅ passed!")
