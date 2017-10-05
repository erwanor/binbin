OPAM_DEPENDS="ounit"


ppa=avsm/ocaml44+opam12

echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
export OPMAYES=1
opam init
opam install ${OPAM_DEPENDS}
eval `opam config env`
make build
make test
