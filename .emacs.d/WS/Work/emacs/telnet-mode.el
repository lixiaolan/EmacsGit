; From zoltan@gizmo.dma.isg.mot.com  Thu Sep 11 10:01:14 1997
; X-VM-v5-Data: ([nil nil nil nil nil t nil nil nil]
; 	[nil "Thu" "11" "September" "1997" "12:49:17" "-0400" "Zoltan Kemenczy" "zoltan@nabu.isg.mot.com" nil "371" "Telnet.el Modification for NT Emacs (19.34.4)" "^From:" nil nil "9" nil nil nil nil]
; 	nil)
; Received: from motgate.mot.com (motgate.mot.com [129.188.136.100]) by june.cs.washington.edu (8.8.5+CS/7.2ju) with ESMTP id KAA00809 for <voelker@cs.washington.edu>; Thu, 11 Sep 1997 10:01:12 -0700
; Received: from pobox.mot.com (pobox.mot.com [129.188.137.100]) by motgate.mot.com (8.8.5/8.6.10/MOT-3.8) with ESMTP id MAA23157; Thu, 11 Sep 1997 12:01:07 -0500 (CDT)
; Comments: ( Received on motgate.mot.com from client pobox.mot.com, sender zoltan@gizmo.dma.isg.mot.com )
; Received: from gizmo.dma.isg.mot.com (gizmo.dma.isg.mot.com [219.1.83.250]) by pobox.mot.com (8.8.5/8.6.10/MOT-3.8) with ESMTP id MAA08688; Thu, 11 Sep 1997 12:01:00 -0500 (CDT)
; Received: from nabu.isg.mot.com (mibs2.ont.isg.mot.com [219.1.83.12]) by gizmo.dma.isg.mot.com (8.7.2/8.7.2) with ESMTP id MAA14204; Thu, 11 Sep 1997 12:51:44 -0400 (EDT)
; Message-Id: <199709111651.MAA14204@gizmo.dma.isg.mot.com>
; From: Zoltan Kemenczy <zoltan@nabu.isg.mot.com>
; To: voelker@cs.washington.edu
; Cc: naftali@harmonic.co.il
; Subject: Telnet.el Modification for NT Emacs (19.34.4)
; Date: Thu, 11 Sep 1997 12:49:17 -0400

; Hi,

; I've played a bit with lisp/telnet.el and got it to work with Naftali's
; telnet.exe if the telnet function is changed as below:

;;;;
(defun telnet (host)
  "Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer `*telnet-HOST*'.
Normally input is edited in Emacs and sent a line at a time."
  (interactive "sOpen telnet connection to host: ")
  (let* ((comint-delimiter-argument-list '(?\  ?\t))
         (name (concat "telnet-" (comint-arguments host 0 nil) ))
	 (buffer (get-buffer (concat "*" name "*")))
	 process)
    (cond ((string-equal system-type "windows-nt")
      (setq telnet-new-line "\n")))
    (if (and buffer (get-buffer-process buffer))
	(pop-to-buffer (concat "*" name "*"))
      (pop-to-buffer (make-comint name telnet-program nil host))
      (setq process (get-buffer-process (current-buffer)))
      (set-process-filter process 'telnet-initial-filter)
      (accept-process-output process)
      (telnet-mode)
      (setq comint-input-sender 'telnet-simple-send)
      (setq telnet-count telnet-initial-count))))

;;*CW 10/27/2000-override telnet-initial-filter defined in telnet.el
;; to enable filter the login prompt, assuming the prompt is "login"
;;;;
(defun telnet-initial-filter (proc string)
  "CW: customized to override the function defined in telnet.el to enable filtering
the login prompt, assuming the prompt is `login'"
  ;For reading up to and including password; also will get machine type.
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (let ((case-fold-search t))
      (cond ((string-match "No such host" string)
	     (kill-buffer (process-buffer proc))
	     (error "No such host"))
	    ((string-match "^login" string)  ;so "Last login xx" will treated as login prompt
	     (telnet-filter proc string)
	     (send-string proc (concat (comint-read-noecho "Login: " t)
				       telnet-new-line))
	     )
	    ((string-match "passw" string)
	     (telnet-filter proc string)
	     (setq telnet-count 0)
	     (send-string proc (concat (comint-read-noecho "Password: " t)
				       telnet-new-line))
	     (clear-this-command-keys))
	    (t (telnet-check-software-type-initialize string)
	       (telnet-filter proc string)
	       (cond ((> telnet-count telnet-maximum-count)
		      (set-process-filter proc 'telnet-filter))
		     (t (setq telnet-count (1+ telnet-count)))))))))

(provide 'telnet-mode)

;;;;

; I'm sorry, I don't have time to figure out how to submit this in a
; "patch" format...

; In summary, the telnet-new-line variable needs to be changed to "\n"
; on NT and the host name has to be passed as a parameter to Naftali's
; telnet.exe instead of sending it an "open" command.

; The same change works well on my HP-UX as well. I'm not an expert, so
; I presume there may be telnet clients on systems out there that cannot
; take the host name as a parameter, so probably the best is to place
; this in a system-type specific load-path.

; I'm going back to my regular duties now...

; Best Regards and THANKS for Emacs on NT/w95,
; Zoltan Kemenczy
; -----------------------------------------------------------------------
; Network Systems Division
; Motorola Information Systems      Fax:   (905) 507-7236
; 400 Matheson Boulevard West       Email: zoltan@nabu.isg.mot.com
; Mississauga, Ontario Canada L5R 3M1
; -----------------------------------------------------------------------

; > From naftali@harmonic.co.il  Sun Jul  6 00:56:49 1997
; > X-VM-v5-Data: ([nil nil nil nil nil nil nil nil nil]
; >         [nil "Sun" " 6" "July" "1997" "10:50:08" "+0300" "Naftali Ramati" "naftali@harmonic.co.il" nil "300" "Re: using telnet under NT emacs" "^From:" nil nil "7" nil nil nil nil]
; >         nil)
; > Received: from sun.harmonic.co.il (sun.harmonic.co.il [192.116.140.3]) by june.cs.washington.edu (8.8.5+CS/7.2ju) with ESMTP id AAA17872 for <voelker@cs.washington.edu>; Sun, 6 Jul 1997 00:56:41 -0700
; > Received: from naftali.harmonic.co.il (modem2.harmonic.co.il [192.116.140.7])   by sun.harmonic.co.il (8.8.5/8.8.5) with SMTP id KAA19065;      Sun, 6 Jul 1997 10:53:24 +0200 (GMT+0200)
; > Message-Id: <199707060853.KAA19065@sun.harmonic.co.il>
; > X-Mailer: Microsoft Outlook Express 4.71.0544.0
; > X-Priority: 3
; > X-MSMail-Priority: Normal
; > MIME-Version: 1.0
; > Content-Type: multipart/alternative;    boundary="----=_NextPart_000_01BC89FA.5F0C4440"
; > X-MimeOLE: Produced By Microsoft MimeOLE Engine V4.71.0544.0
; > From: "Naftali Ramati" <naftali@harmonic.co.il>
; > To: <cmcmahan@Teknowledge.COM>
; > Cc: "Geoff Voelker" <voelker@cs.washington.edu>
; > Subject: Re: using telnet under NT emacs
; > Date: Sun, 6 Jul 1997 10:50:08 +0300
; > 
; > This is a multi-part message in MIME format.
; > 
; > ------=_NextPart_000_01BC89FA.5F0C4440
; > Content-Type: text/plain;
; >         charset="iso-8859-1"
; > Content-Transfer-Encoding: quoted-printable
; > 
; > Looking at the main() in file telnet.cpp, there is a check for valid =
; > 'host' parameter at the start.
; > This check can easily removed, yet  at line 103 of that file, it =
; > automatically try to connect according to that parameter.
; > Instead of using telConnect at that point (line 103) there should be a =
; > code that waits for open... command from the user, then call =
; > telProcessParameters and continue without changes.
; > I'm sorry I can't try that, I had very unpleasant experience installing =
; > Borland C++ V5.0 last time on my computer ( just to deal with this =
; > telnet ), when installed it took about 200MB, and when uninstalled it =
; > takes off only about 150MB, than again, the the uninstall program =
; > crashed. ( It had some more of these 'features')
; > I didn't try to convert it into Visual C++, and I don't have the time =
; > currently.
; > I'm using the program mostly by 'my-telnet' : (only need to change your =
; > domain address in the following routine)
; > 
; > (defun my-telnet()
; >   "invokes telnet in separate comint buffer"
; >   (interactive)
; >   (let ((ip-addr-end 72))
; >     (setq ip-addr-end (read-string "What is the last num of the =
; > ip-address ? "))
; >     (make-telnet ip-addr-end)
; >     ))
; > 
; > (defun make-telnet ( ip-addr-end )
; >   "Creates or Connects to telnet process"
; >     (interactive)
; >   (make-comint (concat "telnet-" ip-addr-end) "telnet" nil
; >                (concat "192\.115\.142\." ip-addr-end))
; >   (switch-to-buffer (concat "*telnet-" ip-addr-end "*"))
; >   (shell-mode)
; >   (abbrev-mode 1)
; >   (insert "
; > ")
; >   (comint-send-input)
; >   )
; > 
; > You can also try to work in 'make-telnet' in (telnet-mode) rather =
; > (shell-mode)
; > Hope that helps,=20
; > Naftali.
; >  ----
; > From: Geoff Voelker <voelker@cs.washington.edu>
; > To: naftali@harmonic.co.il
; > Cc: Chris McMahan <cmcmahan@Teknowledge.COM>
; > Date: Saturday, July 05, 1997 1:36 AM
; > Subject: Fwd: using telnet under NT emacs
; > 
; > Naftali,
; > 
; > Would it be hard to change telnet so that it doesn't need to be
; > started with a host name?
; > 
; > -geoff
; > 
; > ------- start of forwarded message (RFC 934 encapsulation) -------
; > From: Chris McMahan <cmcmahan@Teknowledge.COM>
; > Sender: owner-ntemacs-users@cs.washington.edu
; > To: ntemacs-users@cs.washington.edu
; > Subject: using telnet under NT emacs
; > Date: Thu, 3 Jul 1997 16:38:55 -0700 (PDT)
; > 
; > I went the the FAQ and downloaded the console Telnet application
; > 
; > http://www.cs.washington.edu:/homes/voelker/ntemacs/contrib/telnet.zip
; > 
; > placed the .exe file in my emacs/bin directory, and placed the
; > following line in my .emacs:
; > 
; > ;;;; set up the telnet program
; > (setq telnet-program "d:/emacs/bin/telnet.exe")
; > 
; > 
; > I can use the telnet from within a shell by typing telnet <hostname>,
; > but I cannot get telnet mode to work. I'm getting the error:
; > 
; > > writing to process: invalid argument, telnet-silicon
; > (silicon is the hostname).
; > 
; > I've gone to the telnet.el and (using my LIMITED lisp knowledge)
; > figured out that the lisp launches the telnet program then executes an
; > open <hostname> command.
; > 
; > The problem is that this telnet app will not start without a host name
; > specified on the command line. If one just types telnet, this error
; > appears:
; > 
; > > [4:37pm]g:/Mail> telnet
; > > Usage: (null) <switches> <parameters>
; > >
; > > Switches:
; > 
; > Anybody have this working? Is there an inherent advantage to using
; > telnet mode rather than just telnetting from the shell mode?
; > 
; > Any help would be most appreciated!
; > 
; > Thanks!
; > Chris McMahan
; > - --
; >                   ,,,
; >                  (0 0)
; >   +--------oOO----(_)---------------+
; >   |          Chris McMahan          |
; >   |     cmcmahan@teknowledge.com    |
; >   |     Teknowledge Corporation     |
; >   |    http://www.teknowledge.com   |
; >   |      (415) 424-0500 x488        |
; >   +----------------------oOO--------+
; >       42.7% of all statistics are
; >       made up on the spot.
; > ------- end -------=20
; > 
; > ------=_NextPart_000_01BC89FA.5F0C4440
; > Content-Type: text/html;
; >         charset="iso-8859-1"
; > Content-Transfer-Encoding: quoted-printable
; > 
; > <!DOCTYPE HTML PUBLIC "-//W3C//DTD W3 HTML 3.2//EN">
; > <HTML>
; > <HEAD>
; > <META content=3D"text/html; charset=3Diso-8859-1" =
; > http-equiv=3DContent-Type>
; > <META content=3D'"Trident 4.71.0544.0"' name=3DGENERATOR>
; > 
; > </HEAD>
; > <BODY><FONT face=3DArial size=3D2>
; > <P>Looking at the main() in file telnet.cpp, there is a check for valid =
; > 'host'=20
; > parameter at the start.</P>
; > 
; > <P>This check can easily removed, yet  at line 103 of that file, it=20
; > automatically try to connect according to that parameter.
; > 
; > <P>Instead of using telConnect at that point (line 103) there should be =
; > a code=20
; > that waits for open... command from the user, then call =
; > telProcessParameters and=20
; > continue without changes.
; > 
; > <P>I'm sorry I can't try that, I had very unpleasant experience =
; > installing=20
; > Borland C++ V5.0 last time on my computer ( just to deal with this =
; > telnet ),=20
; > when installed it took about 200MB, and when uninstalled it takes off =
; > only about=20
; > 150MB, than again, the the uninstall program crashed. ( It had some more =
; > of=20
; > these 'features')
; > 
; > <P>I didn't try to convert it into Visual C++, and I don't have the time =
; > 
; > currently.
; > 
; > <P>I'm using the program mostly by 'my-telnet' : (only need to change =
; > your=20
; > domain address in the following routine)
; > 
; > <P><BR>
; > (defun my-telnet()<BR>
; >   &quot;invokes telnet in separate comint buffer&quot;<BR>
; >   (interactive)<BR>
; >   (let ((ip-addr-end 72))<BR>
; >     (setq ip-addr-end (read-string &quot;What is the last num of the =
; > ip-address=20
; > ? &quot;))<BR>
; >     (make-telnet ip-addr-end)<BR>
; >     ))<BR>&nbsp;
; > <BR>
; > (defun make-telnet ( ip-addr-end )<BR>
; >   &quot;Creates or Connects to telnet process&quot;<BR>
; >     (interactive)<BR>
; >   (make-comint (concat &quot;telnet-&quot; ip-addr-end) =
; > &quot;telnet&quot;=20
; > nil<BR>
; >                (concat &quot;192\.115\.142\.&quot; ip-addr-end))<BR>
; >   (switch-to-buffer (concat &quot;*telnet-&quot; ip-addr-end =
; > &quot;*&quot;))<BR>
; >   (shell-mode)<BR>
; >   (abbrev-mode 1)<BR>
; >   (insert &quot;<BR>
; > &quot;)<BR>
; >   (comint-send-input)<BR>
; >   )<BR>&nbsp;
; > <BR>
; > You can also try to work in 'make-telnet' in (telnet-mode) rather =
; > (shell-mode)
; > 
; > <P>Hope that helps,=20
; > 
; > <P>Naftali.</P>
; >  ----<BR>
; > <B>From: </B>Geoff Voelker &lt;voelker@cs.washington.edu&gt;<BR>
; > <B>To: </B>naftali@harmonic.co.il<BR>
; > <B>Cc: </B>Chris McMahan &lt;cmcmahan@Teknowledge.COM&gt;<BR>
; > <B>Date: </B>Saturday, July 05, 1997 1:36 AM<BR>
; > <B>Subject: </B>Fwd: using telnet under NT emacs<BR>
; > <BR>
; > <HTML><BODY><FONT size=3D2>Naftali,<BR>
; > <BR>
; > Would it be hard to change telnet so that it doesn't need to be<BR>
; > started with a host name?<BR>
; > <BR>
; > -geoff<BR>
; > <BR>
; > ------- start of forwarded message (RFC 934 encapsulation) -------<BR>
; > From: Chris McMahan &lt;<A=20
; > href=3D"mailto:cmcmahan@Teknowledge.COM">cmcmahan@Teknowledge.COM</A>&gt;=
; > <BR>
; > Sender: <A=20
; > href=3D"mailto:owner-ntemacs-users@cs.washington.edu">owner-ntemacs-users=
; > @cs.washington.edu</A><BR>
; > To: <A=20
; > href=3D"mailto:ntemacs-users@cs.washington.edu">ntemacs-users@cs.washingt=
; > on.edu</A><BR>
; > Subject: using telnet under NT emacs<BR>
; > Date: Thu, 3 Jul 1997 16:38:55 -0700 (PDT)<BR>
; > <BR>
; > I went the the FAQ and downloaded the console Telnet application<BR>
; > <BR>
; > <A=20
; > href=3D"http://www.cs.washington.edu:/homes/voelker/ntemacs/contrib/telne=
; > t.zip">http://www.cs.washington.edu:/homes/voelker/ntemacs/contrib/telnet=
; > .zip</A><BR>
; > <BR>
; > placed the .exe file in my emacs/bin directory, and placed the<BR>
; > following line in my .emacs:<BR>
; > <BR>
; > ;;;; set up the telnet program<BR>
; > (setq telnet-program &quot;d:/emacs/bin/telnet.exe&quot;)<BR>
; > <BR>
; > <BR>
; > I can use the telnet from within a shell by typing telnet =
; > &lt;hostname&gt;,<BR>
; > but I cannot get telnet mode to work. I'm getting the error:<BR>
; > <BR>
; > &gt; writing to process: invalid argument, telnet-silicon<BR>
; > (silicon is the hostname).<BR>
; > <BR>
; > I've gone to the telnet.el and (using my LIMITED lisp knowledge)<BR>
; > figured out that the lisp launches the telnet program then executes =
; > an<BR>
; > open &lt;hostname&gt; command.<BR>
; > <BR>
; > The problem is that this telnet app will not start without a host =
; > name<BR>
; > specified on the command line. If one just types telnet, this error<BR>
; > appears:<BR>
; > <BR>
; > &gt; [4:37pm]g:/Mail&gt; telnet<BR>
; > &gt; Usage: (null) &lt;switches&gt; &lt;parameters&gt;<BR>
; > &gt;<BR>
; > &gt; Switches:<BR>
; > <BR>
; > Anybody have this working? Is there an inherent advantage to using<BR>
; > telnet mode rather than just telnetting from the shell mode?<BR>
; > <BR>
; > Any help would be most appreciated!<BR>
; > <BR>
; > Thanks!<BR>
; > Chris McMahan<BR>
; > - --<BR>
; > &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&=
; > nbsp;&nbsp;&nbsp;&nbsp;&nbsp;=20
; > ,,,<BR>
; > &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&=
; > nbsp;&nbsp;&nbsp;&nbsp;=20
; > (0 0)<BR>
; > &nbsp; +--------oOO----(_)---------------+<BR>
; > &nbsp; |&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Chris=20
; > McMahan&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; |<BR>
; > &nbsp; |&nbsp;&nbsp;&nbsp;&nbsp; <A=20
; > href=3D"mailto:cmcmahan@teknowledge.com">cmcmahan@teknowledge.com</A>&nbs=
; > p;&nbsp;&nbsp;=20
; > |<BR>
; > &nbsp; |&nbsp;&nbsp;&nbsp;&nbsp; Teknowledge =
; > Corporation&nbsp;&nbsp;&nbsp;&nbsp;=20
; > |<BR>
; > &nbsp; |&nbsp;&nbsp;&nbsp; <A=20
; > href=3D"http://www.teknowledge.com">http://www.teknowledge.com</A>&nbsp;&=
; > nbsp;=20
; > |<BR>
; > &nbsp; |&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; (415) 424-0500=20
; > x488&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; |<BR>
; > &nbsp; +----------------------oOO--------+<BR>
; > &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 42.7% of all statistics are<BR>
; > &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; made up on the spot.<BR>
; > ------- end ------- </FONT></FONT>
; > </BODY></HTML>
; > 
; > ------=_NextPart_000_01BC89FA.5F0C4440--
; > 

