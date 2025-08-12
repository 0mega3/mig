![cover](https://github.com/0mega3/mig/raw/main/doc/src/assets/cover.png)

# MIG

The MIG chat.

## Prerequisites

The installation of prerequisites is shown for
[ArchLinux](https://archlinux.org/).

- [Erlang/OTP](https://www.erlang.org/).

Install the latest version of Erlang/OTP using
[kerl](https://github.com/kerl/kerl) or [asdf](https://asdf-vm.com/)

```sh
yay -S asdf-vm
asdf plugin add erlang https://github.com/asdf-vm/asdf-erlang.git
asdf install erlang latest
```

- [Rebar3](https://rebar3.org/) build tool.

Install the rebar3 package

```sh
sudo pacman -S rebar3
```

## Build

- Update the package index

```sh
rebar3 update
```

- Upgrade dependencies

```sh
rebar3 upgrade --all
```

- Compile the needed dependencies and the project’s apps’ `.app.src`
and `.erl` files

```sh
rebar3 compile
```

- Build release of project

```sh
rebar3 release
```

## Usage

- Start the frontend

  - [WebUI](apps/mig/priv/index.html)

- Start the backend

```sh
./_build/default/rel/mig/bin/mig foreground
```

For more info, see [API documentation](https://hexdocs.pm/mig/readme.html).

## Testing

Run EUnit tests on project apps

```sh
rebar3 eunit
```

## License

This project is based on the original work
[erlang-review](https://github.com/WWWcool/erlang-review) (licensed under the
[Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0)).

Modifications and additions by Denis Khorkin are licensed under the
[BSD-3-Clause License](https://opensource.org/license/bsd-3-clause).

## Credits

- Original work: [WWWcool](https://github.com/WWWcool) (Apache License 2.0)
- Modifications by: [0mega3](https://github.com/0mega3) (BSD-3-Clause)
