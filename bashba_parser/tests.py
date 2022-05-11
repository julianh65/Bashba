import os
import subprocess
from os.path import exists

print("-- Testing scanner --")
print("Rebuilding everything")
os.system("rm scantest.native")
print("Building scanner...")
os.system("ocamlbuild scantest.native")

for i in range(1, 9):
    if("error" in subprocess.getoutput(
            "./scantest.native < ./test_files/t{}".format(i))):
        print("Test case " + str(i) + " ❌ failed")
    else:
        print("Test case " + str(i) + " ✅ passed!")
