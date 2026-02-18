;; Play Fjord Heroes - Nordic Mythology RPG
;; Clarity Version 2, Epoch 2.1

;; CONSTANTS

(define-constant CONTRACT-OWNER tx-sender)

(define-constant ERR-NOT-OWNER (err u100))
(define-constant ERR-NOT-HERO-OWNER (err u101))
(define-constant ERR-HERO-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-TOKENS (err u103))
(define-constant ERR-ALREADY-REGISTERED (err u104))
(define-constant ERR-INVALID-TIER (err u105))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u106))
(define-constant ERR-ALREADY-VOTED (err u107))
(define-constant ERR-PROPOSAL-CLOSED (err u108))
(define-constant ERR-CHEST-NOT-FOUND (err u109))
(define-constant ERR-CHEST-ALREADY-OPENED (err u110))
(define-constant ERR-INVALID-AMOUNT (err u111))

;; Ascension tiers
(define-constant TIER-MORTAL u0)
(define-constant TIER-WARRIOR u1)
(define-constant TIER-LEGEND u2)
(define-constant TIER-GOD u3)

;; Ascension deed thresholds
(define-constant ASCEND-TO-WARRIOR u100)
(define-constant ASCEND-TO-LEGEND u500)
(define-constant ASCEND-TO-GOD u2000)

;; Token mint amounts
(define-constant VALOR-PER-DEED u10)
(define-constant WISDOM-PER-GOVERNANCE u5)
(define-constant FATE-TOKEN-COST u1000) ;; Valor cost to mint one Fate token

;; Chest reward base amounts
(define-constant CHEST-BASE-VALOR u50)
(define-constant CHEST-BASE-WISDOM u20)

;; Proposal voting period (in blocks)
(define-constant VOTING-PERIOD u1440)

;; ============================================================
;; DATA VARS
;; ============================================================

(define-data-var hero-nonce uint u0)
(define-data-var chest-nonce uint u0)
(define-data-var proposal-nonce uint u0)
(define-data-var total-lore-entries uint u0)

;; ============================================================
;; FUNGIBLE TOKENS
;; ============================================================

;; Valor: earned through combat and exploration
(define-fungible-token valor-token)

;; Wisdom: earned through governance participation
(define-fungible-token wisdom-token)

;; Fate: rare token for proposing world-altering events
(define-fungible-token fate-token)

;; ============================================================
;; NON-FUNGIBLE TOKEN - Hero NFT
;; ============================================================

(define-non-fungible-token hero-nft uint)

;; ============================================================
;; DATA MAPS
;; ============================================================

;; Core hero stats
(define-map heroes
  { hero-id: uint }
  {
    owner: principal,
    name: (string-ascii 64),
    tier: uint,
    deeds: uint,
    karma: int,          ;; Karma Ledger: positive or negative reputation
    battles-won: uint,
    quests-completed: uint,
    created-at: uint,    ;; block height
    is-fallen: bool,
    is-legend: bool      ;; true if resurrected as playable legend
  }
)

;; Maps principal to their hero ID (one hero per player for simplicity)
(define-map player-hero
  { player: principal }
  { hero-id: uint }
)

;; Karma Ledger: permanent on-chain action log
(define-map karma-ledger
  { hero-id: uint, entry-index: uint }
  {
    action: (string-ascii 128),
    karma-delta: int,
    block-height: uint
  }
)

(define-map karma-entry-count
  { hero-id: uint }
  { count: uint }
)

;; Lore entries: community-authored mythological stories stored immutably
(define-map lore-entries
  { lore-id: uint }
  {
    author-hero-id: uint,
    title: (string-ascii 128),
    content-hash: (buff 32), ;; keccak/sha256 hash of full content stored off-chain
    block-height: uint,
    upvotes: uint
  }
)

;; Living Rune Chests
(define-map rune-chests
  { chest-id: uint }
  {
    owner: principal,
    hero-id: uint,
    collaboration-score: uint, ;; metric derived from recent player actions
    is-opened: bool,
    created-at: uint
  }
)

;; Governance proposals for world events
(define-map world-proposals
  { proposal-id: uint }
  {
    proposer-hero-id: uint,
    description: (string-ascii 256),
    votes-for: uint,
    votes-against: uint,
    created-at: uint,
    is-executed: bool
  }
)

(define-map proposal-votes
  { proposal-id: uint, voter: principal }
  { voted: bool }
)

;; Cross-chain artifact registry (partner game items recorded here)
(define-map artifacts
  { artifact-id: (buff 32) }
  {
    hero-id: uint,
    name: (string-ascii 64),
    origin-chain: (string-ascii 32),
    power-level: uint,
    provenance-hash: (buff 32)
  }
)

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

(define-private (get-hero-or-err (hero-id uint))
  (match (map-get? heroes { hero-id: hero-id })
    hero (ok hero)
    ERR-HERO-NOT-FOUND
  )
)

(define-private (is-hero-owner (hero-id uint) (caller principal))
  (match (map-get? heroes { hero-id: hero-id })
    hero (is-eq (get owner hero) caller)
    false
  )
)

(define-private (compute-tier (deeds uint))
  (if (>= deeds ASCEND-TO-GOD)
    TIER-GOD
    (if (>= deeds ASCEND-TO-LEGEND)
      TIER-LEGEND
      (if (>= deeds ASCEND-TO-WARRIOR)
        TIER-WARRIOR
        TIER-MORTAL
      )
    )
  )
)

;; Append a Karma Ledger entry for a hero
(define-private (log-karma (hero-id uint) (action (string-ascii 128)) (delta int))
  (let (
    (current-count (default-to u0 (get count (map-get? karma-entry-count { hero-id: hero-id }))))
  )
    (map-set karma-ledger
      { hero-id: hero-id, entry-index: current-count }
      { action: action, karma-delta: delta, block-height: block-height }
    )
    (map-set karma-entry-count
      { hero-id: hero-id }
      { count: (+ current-count u1) }
    )
  )
)

;; ============================================================
;; PUBLIC - HERO REGISTRATION
;; ============================================================

;; Register a new hero NFT for the caller
(define-public (register-hero (name (string-ascii 64)))
  (let (
    (hero-id (+ (var-get hero-nonce) u1))
  )
    (asserts! (is-none (map-get? player-hero { player: tx-sender })) ERR-ALREADY-REGISTERED)
    (try! (nft-mint? hero-nft hero-id tx-sender))
    (map-set heroes
      { hero-id: hero-id }
      {
        owner: tx-sender,
        name: name,
        tier: TIER-MORTAL,
        deeds: u0,
        karma: 0,
        battles-won: u0,
        quests-completed: u0,
        created-at: block-height,
        is-fallen: false,
        is-legend: false
      }
    )
    (map-set player-hero { player: tx-sender } { hero-id: hero-id })
    (var-set hero-nonce hero-id)
    ;; Grant initial Valor tokens to new heroes
    (try! (ft-mint? valor-token u20 tx-sender))
    (log-karma hero-id "Hero born into the mortal realm" 1)
    (ok hero-id)
  )
)

;; ============================================================
;; PUBLIC - GAMEPLAY ACTIONS
;; ============================================================

;; Record a completed battle for the caller's hero
(define-public (record-battle-win)
  (let (
    (hero-id (unwrap! (get hero-id (map-get? player-hero { player: tx-sender })) ERR-HERO-NOT-FOUND))
    (hero (unwrap! (map-get? heroes { hero-id: hero-id }) ERR-HERO-NOT-FOUND))
    (new-deeds (+ (get deeds hero) u10))
    (new-tier (compute-tier new-deeds))
  )
    (map-set heroes { hero-id: hero-id }
      (merge hero {
        deeds: new-deeds,
        battles-won: (+ (get battles-won hero) u1),
        tier: new-tier,
        karma: (+ (get karma hero) 2)
      })
    )
    (try! (ft-mint? valor-token VALOR-PER-DEED tx-sender))
    (log-karma hero-id "Victorious in battle" 2)
    (ok true)
  )
)

;; Record a completed quest for the caller's hero
(define-public (record-quest-completion)
  (let (
    (hero-id (unwrap! (get hero-id (map-get? player-hero { player: tx-sender })) ERR-HERO-NOT-FOUND))
    (hero (unwrap! (map-get? heroes { hero-id: hero-id }) ERR-HERO-NOT-FOUND))
    (new-deeds (+ (get deeds hero) u20))
    (new-tier (compute-tier new-deeds))
  )
    (map-set heroes { hero-id: hero-id }
      (merge hero {
        deeds: new-deeds,
        quests-completed: (+ (get quests-completed hero) u1),
        tier: new-tier,
        karma: (+ (get karma hero) 5)
      })
    )
    (try! (ft-mint? valor-token (* VALOR-PER-DEED u2) tx-sender))
    (log-karma hero-id "Quest completed and lore written" 5)
    (ok true)
  )
)

;; Mark a hero as fallen (defeated in high-stakes combat)
(define-public (mark-hero-fallen (hero-id uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
    (let (
      (hero (unwrap! (map-get? heroes { hero-id: hero-id }) ERR-HERO-NOT-FOUND))
    )
      (map-set heroes { hero-id: hero-id }
        (merge hero { is-fallen: true })
      )
      (log-karma hero-id "Hero has fallen in glorious battle" -10)
      (ok true)
    )
  )
)

;; Resurrect a fallen hero as a playable legend (community governance action)
(define-public (resurrect-as-legend (hero-id uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
    (let (
      (hero (unwrap! (map-get? heroes { hero-id: hero-id }) ERR-HERO-NOT-FOUND))
    )
      (map-set heroes { hero-id: hero-id }
        (merge hero { is-fallen: false, is-legend: true, tier: TIER-LEGEND })
      )
      (log-karma hero-id "Resurrected as immortal legend by community vote" 50)
      (ok true)
    )
  )
)

;; ============================================================
;; PUBLIC - TOKEN ECONOMY
;; ============================================================

;; Burn Valor tokens to mint a rare Fate token
(define-public (convert-valor-to-fate)
  (begin
    (asserts! (>= (ft-get-balance valor-token tx-sender) FATE-TOKEN-COST) ERR-INSUFFICIENT-TOKENS)
    (try! (ft-burn? valor-token FATE-TOKEN-COST tx-sender))
    (try! (ft-mint? fate-token u1 tx-sender))
    (ok true)
  )
)

;; Transfer Valor tokens to another player
(define-public (transfer-valor (amount uint) (recipient principal))
  (begin
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (ft-transfer? valor-token amount tx-sender recipient)
  )
)

;; Transfer Wisdom tokens to another player
(define-public (transfer-wisdom (amount uint) (recipient principal))
  (begin
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (ft-transfer? wisdom-token amount tx-sender recipient)
  )
)

;; ============================================================
;; PUBLIC - LORE SYSTEM
;; ============================================================

;; Publish a lore entry (immutable community story)
(define-public (publish-lore (title (string-ascii 128)) (content-hash (buff 32)))
  (let (
    (hero-id (unwrap! (get hero-id (map-get? player-hero { player: tx-sender })) ERR-HERO-NOT-FOUND))
    (lore-id (var-get total-lore-entries))
  )
    (map-set lore-entries { lore-id: lore-id }
      {
        author-hero-id: hero-id,
        title: title,
        content-hash: content-hash,
        block-height: block-height,
        upvotes: u0
      }
    )
    (var-set total-lore-entries (+ lore-id u1))
    ;; Reward author with Wisdom tokens
    (try! (ft-mint? wisdom-token u10 tx-sender))
    (log-karma hero-id "Authored new lore for the realm" 3)
    (ok lore-id)
  )
)

;; Upvote a lore entry and reward Wisdom to the author
(define-public (upvote-lore (lore-id uint))
  (let (
    (entry (unwrap! (map-get? lore-entries { lore-id: lore-id }) ERR-HERO-NOT-FOUND))
    (author-hero (unwrap! (map-get? heroes { hero-id: (get author-hero-id entry) }) ERR-HERO-NOT-FOUND))
  )
    (map-set lore-entries { lore-id: lore-id }
      (merge entry { upvotes: (+ (get upvotes entry) u1) })
    )
    ;; Reward the original author's owner with Wisdom
    (try! (ft-mint? wisdom-token u1 (get owner author-hero)))
    (ok true)
  )
)

;; ============================================================
;; PUBLIC - LIVING RUNE CHESTS
;; ============================================================

;; Issue a Living Rune Chest to a player (called by game server/owner)
(define-public (issue-rune-chest (recipient principal) (hero-id uint) (collaboration-score uint))
  (let (
    (chest-id (+ (var-get chest-nonce) u1))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
    (map-set rune-chests { chest-id: chest-id }
      {
        owner: recipient,
        hero-id: hero-id,
        collaboration-score: collaboration-score,
        is-opened: false,
        created-at: block-height
      }
    )
    (var-set chest-nonce chest-id)
    (ok chest-id)
  )
)

;; Open a Living Rune Chest and claim rewards scaled by collaboration score
(define-public (open-rune-chest (chest-id uint))
  (let (
    (chest (unwrap! (map-get? rune-chests { chest-id: chest-id }) ERR-CHEST-NOT-FOUND))
  )
    (asserts! (is-eq (get owner chest) tx-sender) ERR-NOT-HERO-OWNER)
    (asserts! (not (get is-opened chest)) ERR-CHEST-ALREADY-OPENED)
    (let (
      (score (get collaboration-score chest))
      ;; Rewards scale linearly with collaboration score (capped multiplier)
      (multiplier (if (> score u10) u10 score))
      (valor-reward (+ CHEST-BASE-VALOR (* multiplier u5)))
      (wisdom-reward (+ CHEST-BASE-WISDOM (* multiplier u2)))
    )
      (map-set rune-chests { chest-id: chest-id }
        (merge chest { is-opened: true })
      )
      (try! (ft-mint? valor-token valor-reward tx-sender))
      (try! (ft-mint? wisdom-token wisdom-reward tx-sender))
      (ok { valor: valor-reward, wisdom: wisdom-reward })
    )
  )
)

;; ============================================================
;; PUBLIC - GOVERNANCE (World Event Proposals)
;; ============================================================

;; Propose a world-altering event using a Fate token
(define-public (propose-world-event (description (string-ascii 256)))
  (let (
    (hero-id (unwrap! (get hero-id (map-get? player-hero { player: tx-sender })) ERR-HERO-NOT-FOUND))
    (proposal-id (+ (var-get proposal-nonce) u1))
  )
    ;; Costs one Fate token to submit a world proposal
    (asserts! (>= (ft-get-balance fate-token tx-sender) u1) ERR-INSUFFICIENT-TOKENS)
    (try! (ft-burn? fate-token u1 tx-sender))
    (map-set world-proposals { proposal-id: proposal-id }
      {
        proposer-hero-id: hero-id,
        description: description,
        votes-for: u0,
        votes-against: u0,
        created-at: block-height,
        is-executed: false
      }
    )
    (var-set proposal-nonce proposal-id)
    (log-karma hero-id "Proposed a world-altering event" 10)
    (ok proposal-id)
  )
)

;; Vote on a world event proposal using Wisdom tokens
(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let (
    (proposal (unwrap! (map-get? world-proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
  )
    (asserts! (is-none (map-get? proposal-votes { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    (asserts! (< (- block-height (get created-at proposal)) VOTING-PERIOD) ERR-PROPOSAL-CLOSED)
    ;; Costs 1 Wisdom token to participate in governance
    (asserts! (>= (ft-get-balance wisdom-token tx-sender) u1) ERR-INSUFFICIENT-TOKENS)
    (try! (ft-burn? wisdom-token u1 tx-sender))
    (map-set proposal-votes { proposal-id: proposal-id, voter: tx-sender } { voted: true })
    (if vote-for
      (map-set world-proposals { proposal-id: proposal-id }
        (merge proposal { votes-for: (+ (get votes-for proposal) u1) })
      )
      (map-set world-proposals { proposal-id: proposal-id }
        (merge proposal { votes-against: (+ (get votes-against proposal) u1) })
      )
    )
    ;; Reward voter with Wisdom for governance participation
    (try! (ft-mint? wisdom-token WISDOM-PER-GOVERNANCE tx-sender))
    (ok true)
  )
)

;; Execute a passed proposal (owner finalizes after voting period)
(define-public (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? world-proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
    (asserts! (not (get is-executed proposal)) ERR-PROPOSAL-CLOSED)
    (asserts! (>= (- block-height (get created-at proposal)) VOTING-PERIOD) ERR-PROPOSAL-NOT-FOUND)
    (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR-INVALID-TIER)
    (map-set world-proposals { proposal-id: proposal-id }
      (merge proposal { is-executed: true })
    )
    (ok true)
  )
)

;; ============================================================
;; PUBLIC - CROSS-CHAIN ARTIFACT REGISTRY
;; ============================================================

;; Register a cross-chain artifact to a hero (owner bridges it in)
(define-public (register-artifact
    (artifact-id (buff 32))
    (hero-id uint)
    (name (string-ascii 64))
    (origin-chain (string-ascii 32))
    (power-level uint)
    (provenance-hash (buff 32)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
    (asserts! (is-none (map-get? artifacts { artifact-id: artifact-id })) ERR-ALREADY-REGISTERED)
    (map-set artifacts { artifact-id: artifact-id }
      {
        hero-id: hero-id,
        name: name,
        origin-chain: origin-chain,
        power-level: power-level,
        provenance-hash: provenance-hash
      }
    )
    (log-karma hero-id "Legendary artifact bridged from partner realm" 5)
    (ok true)
  )
)

;; ============================================================
;; READ-ONLY QUERIES
;; ============================================================

(define-read-only (get-hero (hero-id uint))
  (map-get? heroes { hero-id: hero-id })
)

(define-read-only (get-player-hero (player principal))
  (map-get? player-hero { player: player })
)

(define-read-only (get-karma-entry (hero-id uint) (entry-index uint))
  (map-get? karma-ledger { hero-id: hero-id, entry-index: entry-index })
)

(define-read-only (get-karma-entry-count (hero-id uint))
  (default-to u0 (get count (map-get? karma-entry-count { hero-id: hero-id })))
)

(define-read-only (get-lore-entry (lore-id uint))
  (map-get? lore-entries { lore-id: lore-id })
)

(define-read-only (get-world-proposal (proposal-id uint))
  (map-get? world-proposals { proposal-id: proposal-id })
)

(define-read-only (get-rune-chest (chest-id uint))
  (map-get? rune-chests { chest-id: chest-id })
)

(define-read-only (get-artifact (artifact-id (buff 32)))
  (map-get? artifacts { artifact-id: artifact-id })
)

(define-read-only (get-valor-balance (player principal))
  (ft-get-balance valor-token player)
)

(define-read-only (get-wisdom-balance (player principal))
  (ft-get-balance wisdom-token player)
)

(define-read-only (get-fate-balance (player principal))
  (ft-get-balance fate-token player)
)

(define-read-only (get-hero-tier-name (hero-id uint))
  (match (map-get? heroes { hero-id: hero-id })
    hero (let ((tier (get tier hero)))
      (if (is-eq tier TIER-GOD) (ok "God")
        (if (is-eq tier TIER-LEGEND) (ok "Legend")
          (if (is-eq tier TIER-WARRIOR) (ok "Warrior")
            (ok "Mortal")
          )
        )
      )
    )
    ERR-HERO-NOT-FOUND
  )
)

(define-read-only (get-total-lore-count)
  (var-get total-lore-entries)
)

(define-read-only (get-total-heroes)
  (var-get hero-nonce)
)
