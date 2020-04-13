workspace(name = "impatience")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "com_habito_rules_purescript",
    sha256 = "f89075129567942fd7cf7c63abe6bd392ba7d5e306a3c7b2d72e5019b43c3f6f",
    strip_prefix = "rules_purescript-1-87aa350d93522681bafda7379804e8501fb15e06",
    urls = ["https://github.com/csamak/rules_purescript-1/archive/87aa350d93522681bafda7379804e8501fb15e06.tar.gz"],
)

load(
    "@com_habito_rules_purescript//purescript:repositories.bzl",
    "purescript_repositories",
)

purescript_repositories()

http_archive(
    name = "io_tweag_rules_nixpkgs",
    sha256 = "f5af641e16fcff5b24f1a9ba5d93cab5ad26500271df59ede344f1a56fc3b17d",
    strip_prefix = "rules_nixpkgs-0.6.0",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.6.0.tar.gz"],
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_git_repository", "nixpkgs_package")

nixpkgs_git_repository(
    name = "nixpkgs",
    revision = "19.09",
)

nixpkgs_package(
    name = "nixpkgs_purescript",
    attribute_path = "purescript",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "nixpkgs_tar",
    attribute_path = "gnutar",
    repository = "@nixpkgs",
)

load("@com_habito_rules_purescript//purescript:nixpkgs.bzl", "purescript_nixpkgs_packageset")

register_toolchains("//site:purescript")

purescript_nixpkgs_packageset(
    name = "psc-package",
    base_attribute_path = "purescriptPackages",
    nix_file = "//site:purescript-packages.nix",
    repositories = {"nixpkgs": "@nixpkgs//:default.nix"},
)

load("@psc-package-imports//:packages.bzl", "purescript_import_packages")

purescript_import_packages(base_attribute_path = "purescriptPackages")

# SWITCHED

http_archive(
    name = "rules_haskell",
    sha256 = "56a8e6337df8802f1e0e7d2b3d12d12d5d96c929c8daecccc5738a0f41d9c1e4",
    strip_prefix = "rules_haskell-0.12",
    urls = ["https://github.com/tweag/rules_haskell/archive/v0.12.tar.gz"],
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

load("@rules_haskell//haskell:toolchain.bzl", "rules_haskell_toolchains")

rules_haskell_toolchains(version = "8.6.5")

http_archive(
    name = "alex",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary")
haskell_cabal_binary(name = "alex", srcs = glob(["**"]), visibility = ["//visibility:public"])
    """,
    sha256 = "b77c8a1270767c64e2adb21a6e91ee7cd904ba17edae17bc20fd03da5256e0e3",
    strip_prefix = "alex-3.2.5",
    urls = ["http://hackage.haskell.org/package/alex-3.2.5/alex-3.2.5.tar.gz"],
)

http_archive(
    name = "happy",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary")
haskell_cabal_binary(name = "happy", srcs = glob(["**"]), visibility = ["//visibility:public"])
    """,
    sha256 = "fb9a23e41401711a3b288f93cf0a66db9f97da1ce32ec4fffea4b78a0daeb40f",
    strip_prefix = "happy-1.19.12",
    urls = ["http://hackage.haskell.org/package/happy-1.19.12/happy-1.19.12.tar.gz"],
)

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    local_snapshot = "//:stackage-snapshot.yaml",
    packages = [
        "QuickCheck",
        "aeson",
        "base",
        "base64-bytestring",
        "blaze-html",
        "bytestring",
        "containers",
        "cryptonite",
        "dhall",
        "either",
        "generic-random",
        "hasql",
        "hasql-th",
        "hedgehog",
        "language-javascript",
        "lens",
        "profunctors",
        "servant-blaze",
        "servant-server",
        "servant-swagger",
        "swagger2",
        "tasty",
        "tasty-ant-xml",
        "tasty-discover",
        "tasty-hedgehog",
        "tasty-hspec",
        "tasty-hunit",
        "template-haskell",
        "text",
        "time",
        "transformers",
        "tuple",
        "wai",
        "wai-app-static",
        "wai-extra",
        "wai-logger",
        "warp",
        "zlib",
    ],
    tools = [
        "@alex",
        "@happy",
    ],
)

http_archive(
    name = "io_bazel_rules_docker",
    sha256 = "dc97fccceacd4c6be14e800b2a00693d5e8d07f69ee187babfd04a80a9f8e250",
    strip_prefix = "rules_docker-0.14.1",
    urls = ["https://github.com/bazelbuild/rules_docker/releases/download/v0.14.1/rules_docker-v0.14.1.tar.gz"],
)

load("@io_bazel_rules_docker//repositories:repositories.bzl", container_repositories = "repositories")

container_repositories()

load("@io_bazel_rules_docker//repositories:deps.bzl", container_deps = "deps")

container_deps()

load("@io_bazel_rules_docker//container:container.bzl", "container_pull")

container_pull(
    name = "base_image",
    digest = "sha256:2b0a8e9a13dcc168b126778d9e947a7081b4d2ee1ee122830d835f176d0e2a70",
    registry = "gcr.io",
    repository = "distroless/base",
)

http_archive(
    name = "distroless",
    sha256 = "d6a065fa2be42364e391dd807bc2ed57bfea4a687e84d74ab452edc8fc8eef6d",
    strip_prefix = "distroless-3c6e10c21f508699c5ef7c84dee2ecc08bc8d5a3",
    urls = ["https://github.com/GoogleContainerTools/distroless/archive/3c6e10c21f508699c5ef7c84dee2ecc08bc8d5a3.tar.gz"],
)

load("@distroless//package_manager:package_manager.bzl", "dpkg_list", "dpkg_src", "package_manager_repositories")

package_manager_repositories()

dpkg_src(
    name = "debian_stretch",
    arch = "amd64",
    distro = "stretch",
    sha256 = "56537cedf58e6f08bb3eafef514a20016fbfd227850ab810c43e5ffb00f57427",
    snapshot = "20200411T143911Z",
    url = "http://snapshot.debian.org/archive",
)

dpkg_list(
    name = "package_bundle",
    packages = [
        "libcomerr2",
        "libffi6",
        "libgcc1",
        "libgmp10",
        "libgnutls30",
        "libgssapi-krb5-2",
        "libhogweed4",
        "libidn11",
        "libk5crypto3",
        "libkeyutils1",
        "libkrb5-3",
        "libkrb5support0",
        "libldap-2.4-2",
        "libnettle6",
        "libp11-kit0",
        "libpq5",
        "libsasl2-2",
        "libstdc++6",
        "libtasn1-6",
        "libtinfo5",
        "zlib1g",
    ],
    sources = [
        "@debian_stretch//file:Packages.json",
    ],
)
