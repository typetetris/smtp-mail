let
  # Nixpkgs source to take test environment from.
  nixpkgs-src = import ./nixpkgs-commit.nix;

  # Nixpkgs set with our smtp-mail.
  nixpkgs = import nixpkgs-src {
    config = {
      packageOverrides = super: {
        haskellPackages = super.haskellPackages.override {
          overrides = self: super: {
            smtp-mail = self.callCabal2nix "smtp-mail" ./. {};
          };
        };
      };
    };
  };

  # Certificates for ssl in tests.
  certs = import "${nixpkgs-src}/nixos/tests/common/acme/server/snakeoil-certs.nix";

  # Lets use the existing test machinery of nixos, but with our nixpkgs 
  python-test = import "${nixpkgs-src}/nixos/tests/make-test-python.nix";

in
  python-test {
    name = "smtp-mail";

    machine = { pkgs, ... }: {
      imports = [ "${nixpkgs-src}/nixos/tests/common/user-account.nix" ];
      services.postfix = {
        enable = true;
        enableSubmission = true;
        enableSubmissions = true;
        sslCACert = certs.ca.cert;
        sslCert = certs."acme.test".cert;
        sslKey = certs."acme.test".key;
        submissionsOptions = {
          smtpd_sasl_auth_enable = "yes";
          smtpd_client_restrictions = "permit";
          milter_macro_daemon_name = "ORIGINATING";
        };
      };

      security.pki.certificateFiles = [
        certs.ca.cert
      ];

      networking.extraHosts = ''
        127.0.0.1 acme.test
      '';

      environment.systemPackages = let
        sendTestMail = pkgs.writeScriptBin "send-testmail" ''
        #!${pkgs.bash}/bin/bash
          integration-test --plain
        '';

        sendTestMailSmtps = pkgs.writeScriptBin "send-testmail-smtps" ''
        #!${pkgs.bash}/bin/bash
          integration-test --tls
        '';
      in [ sendTestMail sendTestMailSmtps nixpkgs.haskellPackages.smtp-mail pkgs.openssl ];
    };

    testScript = ''
      machine.wait_for_unit("postfix.service")
      machine.succeed("send-testmail")
      machine.succeed("send-testmail-smtps")
    '';
  }
