#! /usr/bin/env nix-shell 
#! nix-shell ../nix/shell.nix -i bash
[ $(basename $(pwd)) == "fixnix" ] || (echo "Should be executed from the fixnix directory"; exit -1)

fixnix -h > new-USAGE.txt
diff -u USAGE.txt new-USAGE.txt
if [[ "$?" != "0" ]];
then
  read -p "Are you sure? " -n 1 -r
  echo
  if [[ $REPLY =~ ^[Yy]$ ]]
  then
    mv new-USAGE.txt USAGE.txt
  else
    rm new-USAGE.txt
  fi
else
  echo "No difference."
  rm new-USAGE.txt
fi
