let
  # Nixpkgs source to take test environment from.
  nixpkgs-src = import ./nixpkgs-commit.nix;

  # Nixpkgs set with our smtp-mail.
  nixpkgs = import ./nixpkgs.nix;

  # Certificates for ssl in tests.
  certs = import "${nixpkgs-src}/nixos/tests/common/acme/server/snakeoil-certs.nix";

  # Lets use the existing test machinery of nixos, but with our nixpkgs 
  python-test = import "${nixpkgs-src}/nixos/tests/make-test-python.nix";

in
  python-test {
    name = "smtp-mail";

    machine = { pkgs, ... }: {
      imports = [ "${nixpkgs-src}/nixos/tests/common/user-account.nix" ];
      services = {
        postfix = {
          enable = true;
          enableSubmission = true;
          enableSubmissions = true;
          sslCACert = certs.ca.cert;
          sslCert = certs."acme.test".cert;
          sslKey = certs."acme.test".key;
          submissionOptions = {
            smtpd_sasl_auth_enable = "yes";
            smtpd_client_restrictions = "permit";
            milter_macro_daemon_name = "ORIGINATING";
          };
          submissionsOptions = {
            smtpd_sasl_auth_enable = "yes";
            smtpd_client_restrictions = "permit";
            milter_macro_daemon_name = "ORIGINATING";
          };
          destination = [ "localhost" "acme.test" ];
        };
        dovecot2 = {
          enable = true;
          protocols = [ "imap" ];
        };
      };

      security.pki.certificateFiles = [
        certs.ca.cert
      ];

      networking.extraHosts = ''
        127.0.0.1 acme.test
      '';

      environment.systemPackages = let
        testImap = pkgs.writeScriptBin "test-imap" ''
          #!${nixpkgs.python3.interpreter}
          import imaplib
   
          with imaplib.IMAP4('localhost') as imap:
            imap.login('alice', 'foobar')
            imap.select()
            status, refs = imap.search(None, 'ALL')
            assert status == 'OK'
            assert len(refs) == 1
            status, msg = imap.fetch(refs[0], 'BODY[TEXT]')
            assert status == 'OK'
            assert msg[0][1].strip() == b'Hello world!'
        '';
      in [ nixpkgs.haskellPackages.integration-test testImap nixpkgs.python3];
    };

    testScript = ''
      machine.wait_for_unit("postfix.service")
      machine.wait_for_unit("dovecot2.service")
      machine.succeed("integration-test")
      machine.wait_until_fails('[ "$(postqueue -p)" != "Mail queue is empty" ]')
      machine.succeed("test-imap")
    '';
  }
