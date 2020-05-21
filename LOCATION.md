# Locations
A location is parsed like this:

    (<name> '='|'') <mode> <prefix> ':' <git-owner> '/' <git-repo> '/'
        <git-commit>
    
    where
      
      <mode> is the download mode of the location:
          (('?'|'!'|'!!')|'')
      
      <prefix> is the location prefix:
          ('github'|'gh')
      
      <git-commit> is a git commit:
          ('heads/' <head>|'tags/' <tag>|'rev/' <rev>)

Below is a list of defined locations. If the list is incomplete please
file a bug report to https://github.com/kalhauge/fixnix.

## GitHub (github, gh)

    <git-owner> '/' <git-repo> '/' <git-commit>
    
    where
      
      <git-commit> is a git commit:
          ('heads/' <head>|'tags/' <tag>|'rev/' <rev>)

Connect to the github API.

### Examples:

Accesss the tag of a github page.

    $ fixnix github:nixos/nixpkgs/tags/20.03
    
    fetches: https://github.com/nixos/nixpkgs/archive/20.03.tar.gz
    name: nixpkgs_20.03
    do: Unpack

Accesss the revision of a github page.

    $ fixnix github:nixos/nixpkgs/rev/1234abcd
    
    fetches: https://github.com/nixos/nixpkgs/archive/1234abcd.tar.gz
    name: nixpkgs_1234abcd
    do: Unpack

Accesss the branch of a github page. It will use `git ls-remote` to fix
the current revision.

    $ fixnix github:nixos/nixpkgs/heads/nixpkgs-20.03
    
    (unpure)
