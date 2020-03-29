load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "com_habito_rules_purescript",
    strip_prefix = "rules_purescript-c33a2876b752879dc191f505b3f47a97038d8e8d",
    sha256 = "0d0fbf6ebf60cbb99e9bcdfec7044b105c8d089cb05dbda5a9e5add4b3ae0644",
    urls = ["https://github.com/heyhabito/rules_purescript/archive/c33a2876b752879dc191f505b3f47a97038d8e8d.tar.gz"],
)

load(
    "@com_habito_rules_purescript//purescript:repositories.bzl",
    "purescript_repositories",
)

purescript_repositories()

http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-0.6.0",
    sha256 = "f5af641e16fcff5b24f1a9ba5d93cab5ad26500271df59ede344f1a56fc3b17d",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.6.0.tar.gz"],
)

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_git_repository",
    "nixpkgs_package",
)

nixpkgs_git_repository(
    name = "nixpkgs",
    revision = "19.09",
)

nixpkgs_package(
    name = "nixpkgs_purescript",
    repository = "@nixpkgs",
    attribute_path = "purescript",
)

nixpkgs_package(
    name = "nixpkgs_tar",
    repository = "@nixpkgs",
    attribute_path = "gnutar",
)

register_toolchains("//site:purescript")
