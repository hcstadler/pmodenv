# pmodenv

This software is able to take the difference between an older copy of */proc/self/environ* and the current environment, and turn that into environment module lines. The name **pmodenv** comes from *pmod* which stands for *pmodule*. *Pmodules* is the software used by PSI to provide Linux software.

I used the software myself to create initial environment module file lines for the Intel(R) *pmodule*. After that, there's still some editing necessary, so expect some quirks and don't use this without checking all lines for bad content.

Anyway, it might be useful,

HC

## Example

```text
$ cat /proc/self/environ > /tmp/env.txt
$ PATH=${HOME}/bin:${PATH} HELLO=1 pmodenv -p ${HOME} -e _ < /tmp/env.txt
# produced by: pmodenv -p /home/stadler_h -e _
set PREFIX /home/stadler_h
setenv HELLO 1
prepend-path PATH ${PREFIX}/bin
```
