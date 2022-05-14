import os
import subprocess
from os.path import exists

print("-- Testing Semantic Checker --")
print("Rebuilding everything")
os.system("rm scantest2.native")
print("Building scanner...")
os.system("ocamlbuild semanttest.native")

#tests that should pass
print("Testing valid programs\n")
for i in range(1, 3):
    if("error" in subprocess.getoutput(
            "./semanttest.native < ./semant_test_files/passt{}".format(i))):
        print("Test case " + str(i) + " ❌ failed")
    else:
        print("Test case " + str(i) + " ✅ passed!")

print("")
print("----------")
print("")


print("Testing invalid programs\n")
# tests that should fail
for i in range(1, 3):
    output = subprocess.getoutput("./semanttest.native < ./semant_test_files/failt{}".format(i))
    if "error" in output:
        print(output)
        print("Test case " + str(i) + " ✅ passed!")
    else :
        print(output)
        print("Test case " + str(i) + " ❌ failed")

    # if("error" in subprocess.getoutput(
    #         "./scantest.native < ./semant_test_files/failt{}".format(i))):
    #     print("Test case " + str(i) + " ✅ passed!")
    # else:
    #     print("Test case " + str(i) + " ❌ failed")

