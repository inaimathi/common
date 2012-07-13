-module(gpg).

-export([sign/2, verify/1, read_rsa_pubkey/1]).

sign({file, Filename}, Key) ->
    os:cmd(io_lib:format("gpg -u '~s' --armor --sign < ~s", [Key, Filename]));
sign(Message, Key) ->
    TmpFile = common:make_tempname(),
    file:write_file(TmpFile, Message),
    Res = sign({file, TmpFile}, Key),
    file:delete(TmpFile),
    Res.    

%%% This verify function makes the assumption that the message is single-lined, 
%%% and ends with a newline. It will be for our purposes, but don't rely on this 
%%% for general purpose verification.

verify({file, Filename}) -> 
    Res = os:cmd(io_lib:format("gpg -d ~s", [Filename])),
    [Msg, _Timestamp, Sig, []] = re:split(Res, "\n", [{return, list}]),
    case lists:prefix("gpg: Good signature", Sig) of
	true -> GpgPrefixLen = 27,
		Key = lists:sublist(Sig, GpgPrefixLen, length(Sig) - (GpgPrefixLen + 1)),
		[Msg, Key];
	_ -> false
    end;
verify(Message) -> 
    TmpFile = common:make_tempname(),
    file:write_file(TmpFile, Message),
    Res = verify({file, TmpFile}),
    file:delete(TmpFile),
    Res.

read_rsa_pubkey(Filename) ->
    {ok, IoDev} = file:open(Filename, [read]),
    Res = read_rsa_lines(IoDev, []),
    file:close(IoDev),
    Res.

read_rsa_lines(IoDev, Acc) ->
    Skip = fun () -> read_rsa_lines(IoDev, Acc) end,
    Collect = fun (Line) -> read_rsa_lines(IoDev, [Line | Acc]) end,
    case file:read_line(IoDev) of
	{ok, "-----BEGIN PGP PUBLIC KEY BLOCK-----\n"} ->
	    Collect("-----BEGIN RSA PUBLIC KEY-----\n");
	{ok, "-----END PGP PUBLIC KEY BLOCK-----\n"} ->
	    Collect("-----END RSA PUBLIC KEY-----\n");
	{ok, [86,101,114,115,105,111,110,58,32 | _]} -> %% "Version: .*"
	    Skip();
	{ok, [61 | _]} -> %% "=.*"
	    Skip();
	{ok, "\n"} -> 
	    Skip();
	{ok, Line} -> Collect(Line);
	eof -> lists:reverse(Acc)
    end.
