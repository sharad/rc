#!/usr/bin/env ruby
##
## delicious2sqlite.rb
## from: http://snippets.dzone.com/posts/show/290
## from  http://snippets.dzone.com/posts/show/290
require 'rexml/document'
require 'net/http'
require 'net/https'
require 'rubygems'
require 'sqlite3'

# change credentials!
user = 'username'
pass = 'password'

# User-Agent: required for del.icio.us api
agent = 'del.icio.us backup v0.2'
schema = <<EOF
create table bookmarks (
    hash char(32) primary key,
    url varchar(1024),
    title varchar(1024),
    note varchar(2048),
    time timestamp
);
create table tags (hash char(32), tag varchar(1024));
create index ix_tags_hash on tags (hash);
create index ix_tags_tag on tags (tag);
EOF
insert_url = 'insert into bookmarks (hash, url, title, note, time) ' +
             'values (?, ?, ?, ?, ?);'
insert_tag = 'insert into tags (hash, tag) values (?, ?);'

http = Net::HTTP.new('api.del.icio.us', 443)
http.use_ssl = true
xml = http.start { |http|
    req = Net::HTTP::Get.new('/v1/posts/all', {'User-Agent' => agent})
    req.basic_auth(user, pass)
    http.request(req).body
}

db_name = ARGV[0] || Time.now.strftime("%Y-%m-%d.db")
SQLite3::Database.open(db_name).transaction { |db|
    db.execute_batch(schema)
    db.prepare(insert_url) { |url_stmt|
        db.prepare(insert_tag) { |tag_stmt|
            REXML::Document.new(xml).elements.each('posts/post') { |el|
                url_stmt.execute(el.attributes['hash'],
                    el.attributes['href'], el.attributes['description'],
                    el.attributes['extended'], el.attributes['time'])
                el.attributes['tag'].split(' ').each { |tag|
                    tag_stmt.execute(el.attributes['hash'], tag)
                }
            }
        }
    }
}

