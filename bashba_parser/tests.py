import os
import subprocess
from os.path import exists

print("-- Testing scanner --")
print("Rebuilding everything")
os.system("rm scantest.native")
print("Building scanner...")
os.system("ocamlbuild scantest.native")

for i in range(1, 9):
    print("Testing case " + str(i))
    if("error" in subprocess.getoutput(
            "./scantest.native < ./test_files/t{}".format(i))):
        print("❌ Test failed")
    else:
        print("✅ Test passed!")
