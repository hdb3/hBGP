for repo in router bmplib zserv bgprib bgplib session ; do git clone git@github.com:hdb3/${repo}.git ; done
G -ibgplib -ibgprib -isession -izserv/lib -irouter router/Router.hs
