import os
import subprocess
from os.path import exists

print("-- Testing parser --")
print("Rebuilding everything")
os.system("rm parsetest.native")
print("Building parser...")
os.system("ocamlbuild parsetest.native")

for i in range(1, 13):
    if("error" in subprocess.getoutput(
            "./parsetest.native < ./scan_test_files/t{}".format(i))):
        print("Test case " + str(i) + " ❌ failed")
    else:
        print("Test case " + str(i) + " ✅ passed!")

dir = os.getcwd()
print(open(dir + '/parse_test_files/test_guide.txt', 'r').read())