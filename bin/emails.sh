EMAIL_SUBJECT='test'
TO_EMAIL_ADDRESS='slagter.maarten@gmail.com'
FROM_EMAIL_ADDRESS='slagter.maarten@gmail.com'
FRIENDLY_NAME='Maarten'

mailx -v -s "$EMAIL_SUBJECT" -S smtp-use-starttls -S ssl-verify=ignore -S smtp-auth=login -S smtp=smtp://smtp.gmail.com:587 -S from="$FROM_EMAIL_ADDRESS($FRIENDLY_NAME)" -S smtp-auth-user=$FROM_EMAIL_ADDRESS -S smtp-auth-password=$EMAIL_ACCOUNT_PASSWORD -S ssl-verify=ignore -S nss-config-dir=~/.mozilla/firefox/udpytw8z.default $TO_EMAIL_ADDRESS
