// ============================================================
// Disqus: Load only when demanded

var disqus_shortname = "rmculpepper";

var disqus_config = function () {
    this.page.url = disqus_state.page_url || document.URL;
    this.page.identifier = disqus_state.page_url || document.URL;
    this.page.title = document.title;
};

var disqus_state = {
    is_loaded : false,
    page_url : null,
};

function load_disqus(page_url) {
    if (!disqus_state.is_loaded){
        disqus_state.is_loaded = true;
        disqus_state.page_url = page_url;
        var d = document;
        d.getElementById("load-disqus-button").setAttribute("hidden", "hidden");
        var s = d.createElement('script');
        s.async = true;
        s.src = '//' + disqus_shortname + '.disqus.com/embed.js';        
        s.setAttribute('data-timestamp', + new Date());
        d.body.appendChild(s);
    }
}
