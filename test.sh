#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

#Test "-i"
echo -e -n "Test \"-i\"\t\t"
cat tests/curve > tests/test_i.in
cat tests/curve > tests/test_i.out
./flp22-fun -i tests/test_i.in > tests/test_i.real
result=(diff tests/test_i.out tests/test_i.real)
if [[ $result -eq "" ]]; then echo -e "${GREEN}OK${NC}"; else echo -e "${RED}FAIL${NC}"; fi

#generate key
cat tests/curve > tests/test_k.in
./flp22-fun -k tests/test_k.in > tests/key_self

#signature with self generated key
cat tests/curve > tests/test_k.in
cat tests/hash >> tests/test_k.in
cat tests/key_self >> tests/test_k.in
./flp22-fun -s tests/test_k.in > tests/sign_self

#valid signature verification
cat tests/curve > tests/test_k.in
cat tests/hash >> tests/test_k.in
cat tests/key_self >> tests/test_k.in
cat tests/sign_self >> tests/test_k.in
echo True > tests/test_k.out
./flp22-fun -v tests/test_k.in > tests/test_k.real
diff tests/test_k.out tests/test_k.real

#invalid signature verification
cat tests/curve > tests/test_k.in
cat tests/hash >> tests/test_k.in
cat tests/key_self >> tests/test_k.in
cat tests/sign_pre >> tests/test_k.in
echo False > tests/test_k.out
./flp22-fun -v tests/test_k.in > tests/test_k.real
diff tests/test_k.out tests/test_k.real
#signature with provided generated key

#valid signature verification
cat tests/curve > tests/test_k.in
cat tests/hash >> tests/test_k.in
cat tests/key_pre >> tests/test_k.in
cat tests/sign_pre >> tests/test_k.in
echo True > tests/test_k.out
./flp22-fun -v tests/test_k.in > tests/test_k.real
diff tests/test_k.out tests/test_k.real

#invalid signature verification
cat tests/curve > tests/test_k.in
cat tests/hash >> tests/test_k.in
cat tests/key_pre >> tests/test_k.in
cat tests/sign_self >> tests/test_k.in
echo False > tests/test_k.out
./flp22-fun -v tests/test_k.in > tests/test_k.real
diff tests/test_k.out tests/test_k.real