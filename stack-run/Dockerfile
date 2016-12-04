# My version of fpco/stack-run with a TCP wait script installed
FROM fpco/stack-run:lts-7.11
MAINTAINER Eoin Houlihan <ehoulih@tcd.ie>
RUN wget https://raw.githubusercontent.com/vishnubob/wait-for-it/55c54a5abdfb32637b563b28cc088314b162195e/wait-for-it.sh -P /usr/local/bin
RUN chmod +x /usr/local/bin/wait-for-it.sh
