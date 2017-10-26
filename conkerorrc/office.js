
// http://intranet.arubanetworks.com/searchcenter/Pages/Results.aspx?k=sharadqqq&s=All%20Sites

define_webjump("arubaprofile",
               "http://moss-05:8000/Person.aspx?accountname=ARUBANETWORKS\\%s");

define_webjump("bug",
               "https://bugzilla.merunetworks.com/show_bug.cgi?id=%s");

define_webjump("mantis",
               "https://mantis.fortinet.com/mantis/bug_view_page.php?bug_id=%s");

define_webjump("collab",
               "http://sleet2.merunetworks.com:8080/go?page=ReviewDisplay&reviewid=%s");

define_webjump("arubawiki",
               "http://intranet.arubanetworks.com/searchcenter/Pages/Results.aspx?k=%s&s=All%20Sites");

define_webjump("build",
               "http://builds.arubanetworks.com/cgi-bin/re/find_images.html?build=%s");




// Meru
define_webjump("p4cl",
               "http://rain.merunetworks.com:1667/@md=d&cd=//&cl=&ra=s&rc=s&rt=s&c=tYU@/%s?ac=10");

define_webjump("p4describe",
               "http://rain.merunetworks.com:1667/@md=d&cd=//&c=JUj&cl=&ra=s&rc=s&rt=s@/p4%20describe%20%s?ac=160");

define_webjump("mantis-fixforecast",
               "https://mantis.fortinet.com/view_all_set.php?" +
               "type=1&view_type=0&sort=last_updated&dir=DESC&page_number=1&reporter_id=any&" +
               "group_id=any&handler_id=4709&group_type=reporter&qa_assignee_id=any&" +
               "show_severity=any&show_status=50&css_priority=any&review_required=any&" +
               "add_to_rn=&search_type=1&filter=Apply&search=&search_fixforecast=%s&" +
               "eco_checked_in=&escalated_by=&reportsource=any&affectedcustomer=&" +
               "branch_fix_request=&dev_status=any&qa_test_id=&resolution=any");
