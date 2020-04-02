#/bin/bash

# Password lists: 1e4 and 1e5, respectively
# from https://github.com/danielmiessler/SecLists
SOURCE4=/opt/src/SecLists/Passwords/xato-net-10-million-passwords-10000.txt
SOURCE5=/opt/src/SecLists/Passwords/xato-net-10-million-passwords-100000.txt
SOURCE6=/opt/src/SecLists/Passwords/xato-net-10-million-passwords-1000000.txt

SALT="delicious_salt"

# ----------------------------------------
echo '*** Using 1e4 passwords'
echo

echo 'crypt-sha256 (default 5000 rounds)'
cat $SOURCE4 | time racket auth-crypt-sha256.rkt "$SALT" > /dev/null
echo

echo 'pbkdf2-hmac-sha256 (5000 iters)'
cat $SOURCE4 | time racket auth-pbkdf2.rkt 5000 "$SALT" > /dev/null
echo

# # ----------------------------------------
# echo '*** Using 1e5 passwords'
# echo

# echo 'ed25519'
# cat $SOURCE5 | time racket auth-ed25519.rkt > /dev/null
# echo

# echo 'pbkdf2-hmac-sha256 (60 iters)'
# cat $SOURCE5 | time racket auth-pbkdf2.rkt 60 "$SALT" > /dev/null
# echo

# ----------------------------------------
echo '*** Using 1e6 passwords'
echo

echo 'ed25519'
cat $SOURCE6 | time racket auth-ed25519.rkt > /dev/null
echo

echo 'pbkdf2-hmac-sha256 (60 iters)'
cat $SOURCE6 | time racket auth-pbkdf2.rkt 60 "$SALT" > /dev/null
echo

# ============================================================
# Output on my desktop:

# *** Using 1e4 passwords
#
# crypt-sha256 (default 5000 rounds)
# 20.64user 0.06system 0:20.65elapsed 100%CPU (0avgtext+0avgdata 80304maxresident)k
# 80inputs+0outputs (1major+25112minor)pagefaults 0swaps
#
# pbkdf2-hmac-sha256 (5000 iters)
# 21.41user 0.04system 0:21.44elapsed 100%CPU (0avgtext+0avgdata 118608maxresident)k
# 0inputs+0outputs (0major+34616minor)pagefaults 0swaps
#
# *** Using 1e6 passwords
#
# ed25519
# 30.97user 0.05system 0:31.01elapsed 100%CPU (0avgtext+0avgdata 92568maxresident)k
# 24inputs+0outputs (1major+26971minor)pagefaults 0swaps
#
# pbkdf2-hmac-sha256 (60 iters)
# 31.89user 0.29system 0:32.17elapsed 100%CPU (0avgtext+0avgdata 118704maxresident)k
# 0inputs+0outputs (0major+44713minor)pagefaults 0swaps
