# comictrack
TUI application for tracking comics.

# instructions

__WARNING: this is not well coded, and is missing a lot of features, I have built
what I needed as I needed it and as such it poorly architected, inconsistent, and
all around bad. but if you want to track what comics in a series you own and don't
it is functional__.

This is a pretty simple erlang application. You can build it with `rebar3 escriptize`.
You will need a valid [comicvine api key](https://comicvine.gamespot.com/api/). I normally
set up a bash script on my path which sets the env var then calls the escript.

Almost everything has at least basic help, examples are:
```
comictrack help
comictrack volume help
comictrack issue help
```

the simplest things you will likely want to do are
```
comictrack volume add "The Cold Witch"  # add a volume by name
comictrack volume add 164327            # add a volume by ID (good if name has multiple matches)
comictrack volume show 164327           # view details on a volume
comictrack issue own 1109697            # mark an issue as owned
comictrack issue unowned                # show all unowned issues from volumes you are tracking
comictrack volume sync                  # sync all tracked volumes with comicvine
```

currently only the `volume add` command supports name based matching as this is
provided by the comicvine API. I am in the process of replicating the comicvine
name matching to create a consistent experience.

# contributing

feel free to open a PR, I should get to it within a few days. if you aren't comfort
opening a PR then feel free to open an issue, and I will do my best to add it.
