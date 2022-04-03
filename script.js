const e = React.createElement;
const RC = 5; // row constant
let lf = {}; // letter frequency map
let wordList = []; // list of Word objects

// possible Wordle answers
const WORDS = ["cigar","rebut","sissy","humph","awake","blush","focal","evade","naval","serve","heath","dwarf","model","karma","stink","grade","quiet","bench","abate","feign","major","death","fresh","crust","stool","colon","abase","marry","react","batty","pride","floss","helix","croak","staff","paper","unfed","whelp","trawl","outdo","adobe","crazy","sower","repay","digit","crate","cluck","spike","mimic","pound","maxim","linen","unmet","flesh","booby","forth","first","stand","belly","ivory","seedy","print","yearn","drain","bribe","stout","panel","crass","flume","offal","agree","error","swirl","argue","bleed","delta","flick","totem","wooer","front","shrub","parry","biome","lapel","start","greet","goner","golem","lusty","loopy","round","audit","lying","gamma","labor","islet","civic","forge","corny","moult","basic","salad","agate","spicy","spray","essay","fjord","spend","kebab","guild","aback","motor","alone","hatch","hyper","thumb","dowry","ought","belch","dutch","pilot","tweed","comet","jaunt","enema","steed","abyss","growl","fling","dozen","boozy","erode","world","gouge","click","briar","great","altar","pulpy","blurt","coast","duchy","groin","fixer","group","rogue","badly","smart","pithy","gaudy","chill","heron","vodka","finer","surer","radio","rouge","perch","retch","wrote","clock","tilde","store","prove","bring","solve","cheat","grime","exult","usher","epoch","triad","break","rhino","viral","conic","masse","sonic","vital","trace","using","peach","champ","baton","brake","pluck","craze","gripe","weary","picky","acute","ferry","aside","tapir","troll","unify","rebus","boost","truss","siege","tiger","banal","slump","crank","gorge","query","drink","favor","abbey","tangy","panic","solar","shire","proxy","point","robot","prick","wince","crimp","knoll","sugar","whack","mount","perky","could","wrung","light","those","moist","shard","pleat","aloft","skill","elder","frame","humor","pause","ulcer","ultra","robin","cynic","aroma","caulk","shake","dodge","swill","tacit","other","thorn","trove","bloke","vivid","spill","chant","choke","rupee","nasty","mourn","ahead","brine","cloth","hoard","sweet","month","lapse","watch","today","focus","smelt","tease","cater","movie","saute","allow","renew","their","slosh","purge","chest","depot","epoxy","nymph","found","shall","harry","stove","lowly","snout","trope","fewer","shawl","natal","comma","foray","scare","stair","black","squad","royal","chunk","mince","shame","cheek","ample","flair","foyer","cargo","oxide","plant","olive","inert","askew","heist","shown","zesty","hasty","trash","fella","larva","forgo","story","hairy","train","homer","badge","midst","canny","fetus","butch","farce","slung","tipsy","metal","yield","delve","being","scour","glass","gamer","scrap","money","hinge","album","vouch","asset","tiara","crept","bayou","atoll","manor","creak","showy","phase","froth","depth","gloom","flood","trait","girth","piety","payer","goose","float","donor","atone","primo","apron","blown","cacao","loser","input","gloat","awful","brink","smite","beady","rusty","retro","droll","gawky","hutch","pinto","gaily","egret","lilac","sever","field","fluff","hydro","flack","agape","voice","stead","stalk","berth","madam","night","bland","liver","wedge","augur","roomy","wacky","flock","angry","bobby","trite","aphid","tryst","midge","power","elope","cinch","motto","stomp","upset","bluff","cramp","quart","coyly","youth","rhyme","buggy","alien","smear","unfit","patty","cling","glean","label","hunky","khaki","poker","gruel","twice","twang","shrug","treat","unlit","waste","merit","woven","octal","needy","clown","widow","irony","ruder","gauze","chief","onset","prize","fungi","charm","gully","inter","whoop","taunt","leery","class","theme","lofty","tibia","booze","alpha","thyme","eclat","doubt","parer","chute","stick","trice","alike","sooth","recap","saint","liege","glory","grate","admit","brisk","soggy","usurp","scald","scorn","leave","twine","sting","bough","marsh","sloth","dandy","vigor","howdy","enjoy","valid","ionic","equal","unset","floor","catch","spade","stein","exist","quirk","denim","grove","spiel","mummy","fault","foggy","flout","carry","sneak","libel","waltz","aptly","piney","inept","aloud","photo","dream","stale","vomit","ombre","fanny","unite","snarl","baker","there","glyph","pooch","hippy","spell","folly","louse","gulch","vault","godly","threw","fleet","grave","inane","shock","crave","spite","valve","skimp","claim","rainy","musty","pique","daddy","quasi","arise","aging","valet","opium","avert","stuck","recut","mulch","genre","plume","rifle","count","incur","total","wrest","mocha","deter","study","lover","safer","rivet","funny","smoke","mound","undue","sedan","pagan","swine","guile","gusty","equip","tough","canoe","chaos","covet","human","udder","lunch","blast","stray","manga","melee","lefty","quick","paste","given","octet","risen","groan","leaky","grind","carve","loose","sadly","spilt","apple","slack","honey","final","sheen","eerie","minty","slick","derby","wharf","spelt","coach","erupt","singe","price","spawn","fairy","jiffy","filmy","stack","chose","sleep","ardor","nanny","niece","woozy","handy","grace","ditto","stank","cream","usual","diode","valor","angle","ninja","muddy","chase","reply","prone","spoil","heart","shade","diner","arson","onion","sleet","dowel","couch","palsy","bowel","smile","evoke","creek","lance","eagle","idiot","siren","built","embed","award","dross","annul","goody","frown","patio","laden","humid","elite","lymph","edify","might","reset","visit","gusto","purse","vapor","crock","write","sunny","loath","chaff","slide","queer","venom","stamp","sorry","still","acorn","aping","pushy","tamer","hater","mania","awoke","brawn","swift","exile","birch","lucky","freer","risky","ghost","plier","lunar","winch","snare","nurse","house","borax","nicer","lurch","exalt","about","savvy","toxin","tunic","pried","inlay","chump","lanky","cress","eater","elude","cycle","kitty","boule","moron","tenet","place","lobby","plush","vigil","index","blink","clung","qualm","croup","clink","juicy","stage","decay","nerve","flier","shaft","crook","clean","china","ridge","vowel","gnome","snuck","icing","spiny","rigor","snail","flown","rabid","prose","thank","poppy","budge","fiber","moldy","dowdy","kneel","track","caddy","quell","dumpy","paler","swore","rebar",
"scuba","splat","flyer","horny","mason","doing","ozone","amply","molar","ovary","beset","queue","cliff","magic","truce","sport","fritz","edict","twirl","verse","llama","eaten","range","whisk","hovel","rehab","macaw","sigma","spout","verve","sushi","dying","fetid","brain","buddy","thump","scion","candy","chord","basin","march","crowd","arbor","gayly","musky","stain","dally","bless","bravo","stung","title","ruler","kiosk","blond","ennui","layer","fluid","tatty","score","cutie","zebra","barge","matey","bluer","aider","shook","river","privy","betel","frisk","bongo","begun","azure","weave","genie","sound","glove","braid","scope","wryly","rover","assay","ocean","bloom","irate","later","woken","silky","wreck","dwelt","slate","smack","solid","amaze","hazel","wrist","jolly","globe","flint","rouse","civil","vista","relax","cover","alive","beech","jetty","bliss","vocal","often","dolly","eight","joker","since","event","ensue","shunt","diver","poser","worst","sweep","alley","creed","anime","leafy","bosom","dunce","stare","pudgy","waive","choir","stood","spoke","outgo","delay","bilge","ideal","clasp","seize","hotly","laugh","sieve","block","meant","grape","noose","hardy","shied","drawl","daisy","putty","strut","burnt","tulip","crick","idyll","vixen","furor","geeky","cough","naive","shoal","stork","bathe","aunty","check","prime","brass","outer","furry","razor","elect","evict","imply","demur","quota","haven","cavil","swear","crump","dough","gavel","wagon","salon","nudge","harem","pitch","sworn","pupil","excel","stony","cabin","unzip","queen","trout","polyp","earth","storm","until","taper","enter","child","adopt","minor","fatty","husky","brave","filet","slime","glint","tread","steal","regal","guest","every","murky","share","spore","hoist","buxom","inner","otter","dimly","level","sumac","donut","stilt","arena","sheet","scrub","fancy","slimy","pearl","silly","porch","dingo","sepia","amble","shady","bread","friar","reign","dairy","quill","cross","brood","tuber","shear","posit","blank","villa","shank","piggy","freak","which","among","fecal","shell","would","algae","large","rabbi","agony","amuse","bushy","copse","swoon","knife","pouch","ascot","plane","crown","urban","snide","relay","abide","viola","rajah","straw","dilly","crash","amass","third","trick","tutor","woody","blurb","grief","disco","where","sassy","beach","sauna","comic","clued","creep","caste","graze","snuff","frock","gonad","drunk","prong","lurid","steel","halve","buyer","vinyl","utile","smell","adage","worry","tasty","local","trade","finch","ashen","modal","gaunt","clove","enact","adorn","roast","speck","sheik","missy","grunt","snoop","party","touch","mafia","emcee","array","south","vapid","jelly","skulk","angst","tubal","lower","crest","sweat","cyber","adore","tardy","swami","notch","groom","roach","hitch","young","align","ready","frond","strap","puree","realm","venue","swarm","offer","seven","dryer","diary","dryly","drank","acrid","heady","theta","junto","pixie","quoth","bonus","shalt","penne","amend","datum","build","piano","shelf","lodge","suing","rearm","coral","ramen","worth","psalm","infer","overt","mayor","ovoid","glide","usage","poise","randy","chuck","prank","fishy","tooth","ether","drove","idler","swath","stint","while","begat","apply","slang","tarot","radar","credo","aware","canon","shift","timer","bylaw","serum","three","steak","iliac","shirk","blunt","puppy","penal","joist","bunny","shape","beget","wheel","adept","stunt","stole","topaz","chore","fluke","afoot","bloat","bully","dense","caper","sneer","boxer","jumbo","lunge","space","avail","short","slurp","loyal","flirt","pizza","conch","tempo","droop","plate","bible","plunk","afoul","savoy","steep","agile","stake","dwell","knave","beard","arose","motif","smash","broil","glare","shove","baggy","mammy","swamp","along","rugby","wager","quack","squat","snaky","debit","mange","skate","ninth","joust","tramp","spurn","medal","micro","rebel","flank","learn","nadir","maple","comfy","remit","gruff","ester","least","mogul","fetch","cause","oaken","aglow","meaty","gaffe","shyly","racer","prowl","thief","stern","poesy","rocky","tweet","waist","spire","grope","havoc","patsy","truly","forty","deity","uncle","swish","giver","preen","bevel","lemur","draft","slope","annoy","lingo","bleak","ditty","curly","cedar","dirge","grown","horde","drool","shuck","crypt","cumin","stock","gravy","locus","wider","breed","quite","chafe","cache","blimp","deign","fiend","logic","cheap","elide","rigid","false","renal","pence","rowdy","shoot","blaze","envoy","posse","brief","never","abort","mouse","mucky","sulky","fiery","media","trunk","yeast","clear","skunk","scalp","bitty","cider","koala","duvet","segue","creme","super","grill","after","owner","ember","reach","nobly","empty","speed","gipsy","recur","smock","dread","merge","burst","kappa","amity","shaky","hover","carol","snort","synod","faint","haunt","flour","chair","detox","shrew","tense","plied","quark","burly","novel","waxen","stoic","jerky","blitz","beefy","lyric","hussy","towel","quilt","below","bingo","wispy","brash","scone","toast","easel","saucy","value","spice","honor","route","sharp","bawdy","radii","skull","phony","issue","lager","swell","urine","gassy","trial","flora","upper","latch","wight","brick","retry","holly","decal","grass","shack","dogma","mover","defer","sober","optic","crier","vying","nomad","flute","hippo","shark","drier","obese","bugle","tawny","chalk","feast","ruddy","pedal","scarf","cruel","bleat","tidal","slush","semen","windy","dusty","sally","igloo","nerdy","jewel","shone","whale","hymen","abuse","fugue","elbow","crumb","pansy","welsh","syrup","terse","suave","gamut","swung","drake","freed","afire","shirt","grout","oddly","tithe","plaid","dummy","broom","blind","torch","enemy","again","tying","pesky","alter","gazer","noble","ethos","bride","extol","decor","hobby","beast","idiom","utter","these","sixth","alarm","erase","elegy","spunk","piper","scaly","scold","hefty","chick","sooty","canal","whiny","slash","quake","joint","swept","prude","heavy","wield","femme","lasso","maize","shale","screw","spree","smoky","whiff","scent","glade","spent","prism","stoke","riper","orbit","cocoa","guilt","humus","shush","table","smirk","wrong","noisy","alert","shiny","elate","resin","whole","hunch","pixel",
"polar","hotel","sword","cleat","mango","rumba","puffy","filly","billy","leash","clout","dance","ovate","facet","chili","paint","liner","curio","salty","audio","snake","fable","cloak","navel","spurt","pesto","balmy","flash","unwed","early","churn","weedy","stump","lease","witty","wimpy","spoof","saner","blend","salsa","thick","warty","manic","blare","squib","spoon","probe","crepe","knack","force","debut","order","haste","teeth","agent","widen","icily","slice","ingot","clash","juror","blood","abode","throw","unity","pivot","slept","troop","spare","sewer","parse","morph","cacti","tacky","spool","demon","moody","annex","begin","fuzzy","patch","water","lumpy","admin","omega","limit","tabby","macho","aisle","skiff","basis","plank","verge","botch","crawl","lousy","slain","cubic","raise","wrack","guide","foist","cameo","under","actor","revue","fraud","harpy","scoop","climb","refer","olden","clerk","debar","tally","ethic","cairn","tulle","ghoul","hilly","crude","apart","scale","older","plain","sperm","briny","abbot","rerun","quest","crisp","bound","befit","drawn","suite","itchy","cheer","bagel","guess","broad","axiom","chard","caput","leant","harsh","curse","proud","swing","opine","taste","lupus","gumbo","miner","green","chasm","lipid","topic","armor","brush","crane","mural","abled","habit","bossy","maker","dusky","dizzy","lithe","brook","jazzy","fifty","sense","giant","surly","legal","fatal","flunk","began","prune","small","slant","scoff","torus","ninny","covey","viper","taken","moral","vogue","owing","token","entry","booth","voter","chide","elfin","ebony","neigh","minim","melon","kneed","decoy","voila","ankle","arrow","mushy","tribe","cease","eager","birth","graph","odder","terra","weird","tried","clack","color","rough","weigh","uncut","ladle","strip","craft","minus","dicey","titan","lucid","vicar","dress","ditch","gypsy","pasta","taffy","flame","swoop","aloof","sight","broke","teary","chart","sixty","wordy","sheer","leper","nosey","bulge","savor","clamp","funky","foamy","toxic","brand","plumb","dingy","butte","drill","tripe","bicep","tenor","krill","worse","drama","hyena","think","ratio","cobra","basil","scrum","bused","phone","court","camel","proof","heard","angel","petal","pouty","throb","maybe","fetal","sprig","spine","shout","cadet","macro","dodgy","satyr","rarer","binge","trend","nutty","leapt","amiss","split","myrrh","width","sonar","tower","baron","fever","waver","spark","belie","sloop","expel","smote","baler","above","north","wafer","scant","frill","awash","snack","scowl","frail","drift","limbo","fence","motel","ounce","wreak","revel","talon","prior","knelt","cello","flake","debug","anode","crime","salve","scout","imbue","pinky","stave","vague","chock","fight","video","stone","teach","cleft","frost","prawn","booty","twist","apnea","stiff","plaza","ledge","tweak","board","grant","medic","bacon","cable","brawl","slunk","raspy","forum","drone","women","mucus","boast","toddy","coven","tumor","truer","wrath","stall","steam","axial","purer","daily","trail","niche","mealy","juice","nylon","plump","merry","flail","papal","wheat","berry","cower","erect","brute","leggy","snipe","sinew","skier","penny","jumpy","rally","umbra","scary","modem","gross","avian","greed","satin","tonic","parka","sniff","livid","stark","trump","giddy","reuse","taboo","avoid","quote","devil","liken","gloss","gayer","beret","noise","gland","dealt","sling","rumor","opera","thigh","tonga","flare","wound","white","bulky","etude","horse","circa","paddy","inbox","fizzy","grain","exert","surge","gleam","belle","salvo","crush","fruit","sappy","taker","tract","ovine","spiky","frank","reedy","filth","spasm","heave","mambo","right","clank","trust","lumen","borne","spook","sauce","amber","lathe","carat","corer","dirty","slyly","affix","alloy","taint","sheep","kinky","wooly","mauve","flung","yacht","fried","quail","brunt","grimy","curvy","cagey","rinse","deuce","state","grasp","milky","bison","graft","sandy","baste","flask","hedge","girly","swash","boney","coupe","endow","abhor","welch","blade","tight","geese","miser","mirth","cloud","cabal","leech","close","tenth","pecan","droit","grail","clone","guise","ralph","tango","biddy","smith","mower","payee","serif","drape","fifth","spank","glaze","allot","truck","kayak","virus","testy","tepee","fully","zonal","metro","curry","grand","banjo","axion","bezel","occur","chain","nasal","gooey","filer","brace","allay","pubic","raven","plead","gnash","flaky","munch","dully","eking","thing","slink","hurry","theft","shorn","pygmy","ranch","wring","lemon","shore","mamma","froze","newer","style","moose","antic","drown","vegan","chess","guppy","union","lever","lorry","image","cabby","druid","exact","truth","dopey","spear","cried","chime","crony","stunk","timid","batch","gauge","rotor","crack","curve","latte","witch","bunch","repel","anvil","soapy","meter","broth","madly","dried","scene","known","magma","roost","woman","thong","punch","pasty","downy","knead","whirl","rapid","clang","anger","drive","goofy","email","music","stuff","bleep","rider","mecca","folio","setup","verso","quash","fauna","gummy","happy","newly","fussy","relic","guava","ratty","fudge","femur","chirp","forte","alibi","whine","petty","golly","plait","fleck","felon","gourd","brown","thrum","ficus","stash","decry","wiser","junta","visor","daunt","scree","impel","await","press","whose","turbo","stoop","speak","mangy","eying","inlet","crone","pulse","mossy","staid","hence","pinch","teddy","sully","snore","ripen","snowy","attic","going","leach","mouth","hound","clump","tonal","bigot","peril","piece","blame","haute","spied","undid","intro","basal","shine","gecko","rodeo","guard","steer","loamy","scamp","scram","manly","hello","vaunt","organ","feral","knock","extra","condo","adapt","willy","polka","rayon","skirt","faith","torso","match","mercy","tepid","sleek","riser","twixt","peace","flush","catty","login","eject","roger","rival","untie","refit","aorta","adult","judge","rower","artsy","rural","shave"];

// populate lf
let letters = ["a","b","c","d","e","f","g","h","i","j","k","l","m",
    "n","o","p","q","r","s","t","u","v","w","x","y","z"];
let occurrences = [3615,1023,1403,1615,3993,707,1050,1185,2509,184,913,2231,1267,
    1912,2626,1301,79,2751,4106,2137,1655,465,686,212,1371,227];
    // credit: https://leancrew.com/all-this/2022/01/wordle-letters/ for letter frequency analysis
for (let i = 0; i < 26; i++) {
    lf[letters[i]] = occurrences[i];
}

/**
 * Store the word as a letter-separated array, with a corresponding
 * integer score dependent on the frequency of its letters in the Wordle list.
 */
class Word {
    constructor(word) {
        this.letters = [];
        this.score = 0;
        for (let i = 0; i < 5; i++) {
            let letter = word.charAt(i);
            this.score += this.letters.includes(letter) ? 0 : lf[letter];
            this.letters.push(letter);
        }
    }
}

/**
 * Store word objects in a list sorted by word scores.
 * Initialize the list using every Wordle word,
 * and provide methods to whittle down the list
 * based on guess information.
 */
class WordList {
    constructor() {
        this.words = [];
        for (let i = 0; i < WORDS.length; i++) {
            this.words.push(new Word(WORDS[i]));
        }
        this.words.sort(function(a,b){return b.score-a.score;});
    }

    reset() {
        this.words = [];
        for (let i = 0; i < WORDS.length; i++) {
            this.words.push(new Word(WORDS[i]));
        }
        this.words.sort(function(a,b){return b.score-a.score;});
    }

    update(letters, colors) {
        let letmap = new Array(26).fill(0);
        for (let i = 0; i < 5; i++) {
            letmap[letters[i].charCodeAt(0) - 97]++;
        }
        for (let i = 0; i < 5; i++) {
            let s = letters[i];
            let int = colors[i];
            let duplicatePresent = false;
            if (int == 1) { // yellow
                this.words = this.words.filter(word => word.letters.includes(s));
                this.words = this.words.filter(word => word.letters[i] != s);
                if (letmap[letters[i].charAt(0) - 97] > 0) {
                    duplicatePresent = true;
                }
            } else if (int == 2) { // green
                this.words = this.words.filter(word => word.letters[i] == s);
                if (letmap[letters[i].charAt(0) - 97] > 0) {
                    duplicatePresent = true;
                }
            } else { // black
                if (letmap[letters[i].charCodeAt(0) - 97] > 0 && duplicatePresent) {
                    this.words = this.words.filter(word => (word.letters[i] != s));
                } else {
                    this.words = this.words.filter(word => (!word.letters.includes(s)));
                }
            }
        }
    }
    
    getGuess() {
        let guesses = [];
        for (let i = 0; i < 5 && i < this.words.length; i++) {
            guesses.push(this.words[i].letters.join(''));
        }
        if (guesses.length == 0) {
            return "There are no possible words to choose from. Try again?";
        } else if (guesses.length == 1) {
            return "Congratulations! The Wordle is " + guesses.join() + "!";
        } else if (this.words.length > 5) {
            return "There are " + this.words.length + " possible words, but here are the top "
                + guesses.length + ":\n\u2022 " + guesses.join("\n\u2022 ");
        }
        return "There are " + guesses.length + " possible words. " +
            "They are:\n\u2022 " + guesses.join("\n\u2022 ");
    }
}

let ulist = new WordList(); // user dictionary

function Square(props) {
    return e ("div", {
        onClick: () => props.onClick(),
        className: !props.clicked ? "ltr blank" :
            (props.color == 0 ? "ltr black" :
            (props.color == 1 ? "ltr yellow" : "ltr green"))
    }, props.letter);
}

function Output(props) {
    return e ("div", {className: "output"}, props.val);
}

class Board extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            letters: Array(30).fill(""),
            colors: Array(30).fill(-1),
            clicked: Array(30).fill(false),
            currentRow: 0,
            submitReady: false,
            output: ulist.getGuess()
        };
        this.handleKeyPress = this.handleKeyPress.bind(this);
    }

    handleClick(i) {
        if (i >= RC * this.state.currentRow && i <= RC * this.state.currentRow + (RC - 1)) {
            const clicked = this.state.clicked.slice();
            const letters = this.state.letters.slice();
            const row = this.state.currentRow;
            let updateSubmit = true;
            if (clicked[i] == false && letters[i] != "") {
                clicked[i] = true;
            }
            if (clicked[i]) {
                const colors = this.state.colors.slice();
                colors[i] = (colors[i] + 1) % 3;
                this.setState({clicked: clicked, colors: colors});
            }
            for (let i = RC * row; i < RC * row + RC; i++) {
                if (!clicked[i]) {
                    updateSubmit = false;
                }
            }
            if (updateSubmit) {
                this.setState({submitReady: true});
            }
        }
    }

    componentDidMount() {
        window.addEventListener('keydown', this.handleKeyPress);
    }
    componentWillUnmount() {
        window.removeEventListener('onkeydown', this.handleKeyPress);
    }
    handleKeyPress(event) {
        const letters = this.state.letters.slice();
        const row = this.state.currentRow;
        const k = event.key;
        if (k.length == 1 && ((k.charCodeAt(0) >= 97 && k.charCodeAt(0) <= 122)
            || (k.charCodeAt(0) >= 65 && k.charCodeAt(0) <= 90))) { // a-z, A-Z
            if (letters[RC - 1 + RC * row] == "") { // have capacity
                let i = RC * row;
                while (letters[i] != "" && i < RC * (row + 1) - 1) {
                    i++;
                }
                letters[i] = k.toLowerCase();
                this.setState({letters: letters});
            }
        } else if (k == "Backspace") {
            if (letters[RC * row] != "") { // capacity to delete
                let i = RC * row + RC - 1;
                while (letters[i] == "") {
                    i--;
                }
                letters[i] = "";
                const clicked = this.state.clicked.slice();
                clicked[i] = false;
                const colors = this.state.colors.slice();
                colors[i] = -1;
                this.setState({
                    letters: letters,
                    clicked: clicked,
                    colors: colors,
                    submitReady: false
                });
            }
        }
    }

    handleSubmit() {
        const clicked = this.state.clicked;
        const row = this.state.currentRow;
        const letters = this.state.letters.slice(RC * row, RC * row + RC);
        const colors = this.state.colors.slice(RC * row, RC * row + RC);
        let updateRow = true;
        for (let i = RC * row; i < RC * row + RC; i++) {
            if (!clicked[i]) {
                updateRow = false;
            }
        }
        if (updateRow) {
            ulist.update(letters, colors);
            this.setState({
                output: ulist.getGuess(),
                currentRow: row + 1,
                submitReady: false
            });
        }
    }

    renderSquare(i) {
        return e (Square, {
            clicked: this.state.clicked[i],
            letter: this.state.letters[i],
            color: this.state.colors[i],
            onClick: () => this.handleClick(i),
        });
    }

    renderBoard() {
        return e ("div", {className: "letters"},
            this.renderSquare(0),
            this.renderSquare(1),
            this.renderSquare(2),
            this.renderSquare(3),
            this.renderSquare(4),
            this.renderSquare(5),
            this.renderSquare(6),
            this.renderSquare(7),
            this.renderSquare(8),
            this.renderSquare(9),
            this.renderSquare(10),
            this.renderSquare(11),
            this.renderSquare(12),
            this.renderSquare(13),
            this.renderSquare(14),
            this.renderSquare(15),
            this.renderSquare(16),
            this.renderSquare(17),
            this.renderSquare(18),
            this.renderSquare(19),
            this.renderSquare(20),
            this.renderSquare(21),
            this.renderSquare(22),
            this.renderSquare(23),
            this.renderSquare(24),
            this.renderSquare(25),
            this.renderSquare(26),
            this.renderSquare(27),
            this.renderSquare(28),
            this.renderSquare(29)
        );
    }

    renderOutput() {
        return e (Output, {val: this.state.output});
    }

    render() {
        return e ("div", null,
            this.renderBoard(),
            e ("button", {
                className: this.state.submitReady ? "submit enabled" : "submit disabled",
                onClick: () => this.handleSubmit()
            }, "SUBMIT"),
            this.renderOutput()
        );
    }
}

ReactDOM.render(e(Board, null), document.getElementById("root"));
