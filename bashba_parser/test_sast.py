import os
import subprocess
from os.path import exists

print("-- Testing Semantic Checker --")
print("Rebuilding everything")
os.system("rm scantest2.native")
print("Building scanner...")
os.system("ocamlbuild semanttest.native")

#tests that should pass
for i in range(1, 2):
    if("error" in subprocess.getoutput(
            "./scantest.native < ./semant_test_files/t{}".format(i))):
        print("Test case " + str(i) + " ❌ failed")
    else:
        print("Test case " + str(i) + " ✅ passed!")

# tests that should fail
for i in range(1, 2):
    if("error" in subprocess.getoutput(
            "./scantest.native < ./semant_test_files/t{}".format(i))):
        print("Test case " + str(i) + " ✅ passed!")
    else:
        print("Test case " + str(i) + " ❌ failed")

