# Terraform HTTP Backend Pass

**Catchy name! What does it do?**

According to [terraform
docs](https://www.terraform.io/docs/language/settings/backends/index.html):

> Each Terraform configuration can specify a backend, which defines where and
> how operations are performed, where state snapshots are stored, etc.

There are a few backends which terraform provides, none of which are very
customizable. The [http
backend](https://www.terraform.io/docs/language/settings/backends/http.html),
however, provides a way to define where (and how) state snapshots are stored. It
additionally allows for a locking the state while some operation is being
perfomed.

This project aims to provide a backend to store the terraform state in a git
repository. As the state can contain sensitive information, it should be
encrypted before storing, for which [`pass`](https://www.passwordstore.org/) is
used.

## How to use

1. Install the backend in one of these ways:
   - Using [Stack](https://docs.haskellstack.org/en/stable/README/):
     ```bash
     stack install terraform-http-backend-pass
     ```
   - Using [Cabal](https://www.haskell.org/cabal/):
     ```bash
     cabal install terraform-http-backend-pass
     ```
   - Using [nix](https://nixos.org/):
     ```bash
     nix-env -f https://git.coop/akshay/terraform-http-backend-pass/-/archive/main/terraform-http-backend-pass-main.tar.gz -i
     ```
2. Create a pass repository:
   ```bash
   export PASSWORD_STORE_DIR=/desired/path/to/store
   pass init <gpg-keys>
   pass git init
   ```
3. Push the repository somewhere, set push upstream:
   ```bash
   export PASSWORD_STORE_DIR=/desired/path/to/store
   pass git remote add origin <remote-url>
   pass git push -u origin master
   ```
4. Start the backend:
   ```bash
   terraform-http-backend-pass --repositoryPath /desired/path/to/store --port 8888 
   ```
5. Setup terraform with backend information:
   ```tf
   terraform {
     backend "http" {
       # Or, something else if the server is not running on localhost
       address = "http://localhost:8888"
     }
   }
   ```
   The address can also be specified dynamically using the `-backend-config`
   option while running `terraform init`
6. Use terraform as usual:
   ```bash
   terraform init
   terraform apply
   ```
