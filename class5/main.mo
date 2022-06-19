import Result "mo:base/Result";
import Principal "mo:base/Principal";
import Cycles "mo:base/ExperimentalCycles";
import Array "mo:base/Array";
import TrieMap "mo:base/TrieMap";
import Time "mo:base/Time";
import Prim "mo:⛔";
import Iter "mo:base/Iter";
import Account "Account";
import Blob "mo:base/Blob";
import Nat "mo:base/Nat";
import Hash "mo:base/Hash";
import Option "mo:base/Option";

shared(installer) actor class hub(m : Nat, members: [Principal]) = this{
    private stable var M  = m;
    type Error = {
        #Invalid_Caller;
        #Invalid_CanisterId;
        #No_Wasm;
        #No_Record;
        #Insufficient_Cycles;
        #Transfer_Failed;
        #Invalid_Propose_Result;
        #Invalid_Propose_index;
    };
    type Action = {
      #Install;
      #Add_Owner;
      #Del_Owner;
      #Create_Canister;
      #Start_Canister;
      #Stop_Canister;
      #Del_Canister;
    };
    type Canister = {
        name : Text;
        description : Text;
        canister_id : Principal;
        wasm : ?[Nat8];
    };
    type Propose = {
      index : Nat;
      file_key : Text;
      principal : Principal;
      content : Text; 
      action : Action;
      wasm : ?[Nat8];
      result : Bool;
    };

    type Record = {
        canister_id : Principal;
        method : {#deploy; #deposit; #start; #stop;};
        amount : Nat;
        times : Time.Time;
    };


    stable var record_entries : [(Principal,[Record])] = [];
    var records : TrieMap.TrieMap<Principal,[Record]> = TrieMap.fromEntries(record_entries.vals(), Principal.equal, Principal.hash);

    stable var owner : Principal = installer.caller;
    stable var owners : [Principal] = members;
    stable var proposes_entries : [(Nat, Propose)] = [];
    stable var vote_entries : [(Nat, Nat)] = [];
    var proposes : TrieMap.TrieMap<Nat, Propose> = TrieMap.fromEntries(proposes_entries.vals(), Nat.equal, Hash.hash);
    stable var canisters_entries : [(Principal, Canister)] = [];
    var canisters : TrieMap.TrieMap<Principal, Canister> = TrieMap.fromEntries(canisters_entries.vals(), Principal.equal, Principal.hash);
    var vote : TrieMap.TrieMap<Nat, Nat> = TrieMap.fromEntries(vote_entries.vals(), Nat.equal, Hash.hash);

    public shared({caller}) func changeOwner(newOwner : Principal) : async Result.Result<(), Error>{
        if(caller == owner){
            owner := newOwner;
            #ok()
        }else{
            #err(#Invalid_Caller)
        }
    };

    public query func getOwner() : async Principal{
        owner
    };

    stable var cycle_wasm : [Nat8] = [];

    public shared({caller}) func installCycleWasm(wasm : [Nat8]) : async (){
        if(caller == installer.caller){
            cycle_wasm := wasm
        }
    };

    type Status = {
        cycle_balance : Nat;
        memory : Nat;
    };

    public query({caller}) func getStatus() : async Result.Result<Status, Error>{
        if(caller != owner){
            return #err(#Invalid_Caller)
        };
        #ok({
            cycle_balance = Cycles.balance();
            memory = Prim.rts_memory_size()
        })
    };
    
    public query({caller}) func getOwners() : async Result.Result<[Principal],Error>{
        switch(Array.find(owners,func(id : Principal) : Bool {id == caller})){
           case null return #err(#Invalid_Caller);
           case (?c) {
             #ok(owners);
           }
         };
    };

    public query({caller}) func getCanisters() : async Result.Result<[Canister], Error>{
        var res = Array.init<Canister>(canisters.size(), {
            name = "";
            description = "";
            canister_id = Principal.fromActor(this);
            wasm = null;
        });
        var index = 0;
        for(c in canisters.vals()){
            res[index] := c;
            index := index + 1;
        };
        #ok(Array.freeze<Canister>(res))
    };
    public query({caller}) func getProposes() : async Result.Result<[Propose], Error>{
        var res = Array.init<Propose>(proposes.size(), {
            index = 0;
            file_key = "";
            principal = Principal.fromActor(this);
            content = "";
            action = #Install;
            wasm = ?[0];
            result = false;
        });
        var index = 0;
        for(c in proposes.vals()){
            res[index] := c;
            index := index + 1;
        };
        #ok(Array.freeze<Propose>(res))
    };
    public query({caller}) func getRecords(p : Principal) : async Result.Result<[Record], Error>{
        if(caller != owner){
            return #err(#Invalid_Caller)
        };
        switch(records.get(p)){
            case null { #err(#No_Record)};
            case (?r) { 
                #ok(r)
            }
        };

    };

    public query({caller}) func getWasm(canister_id : Principal) : async Result.Result<[Nat8], Error>{
        if(caller != owner){
            return #err(#Invalid_Caller)
        };
        switch(canisters.get(canister_id)){
            case null { #err(#Invalid_CanisterId) };
            case(?c){
                switch(c.wasm){
                    case null { #err(#No_Wasm) };
                    case(?wasm){
                        #ok(wasm)
                    }
                }
            }
        }
    };



    public shared({caller}) func addPropose(p: Propose) : async Result.Result<Nat, Error> {
         switch(Array.find(owners,func(id : Principal) : Bool {id == caller})){
           case null return #err(#Invalid_Caller);
           case (?c) {
             var tmp : Propose = {
                index = proposes.size();
                file_key = p.file_key;
                principal = p.principal;
                content = p.content;
                action = p.action;
                wasm = p.wasm;  
                result = false;
             };
             proposes.put(proposes.size() + 1, tmp);
             #ok(proposes.size() + 1)
           }
         };
    };

    private func eqId(id: Principal) : Principal -> Bool {
      func (id) { id == id }
    }; 

    public shared({caller}) func votePropose(index : Nat, agree : Bool) : async Result.Result<(), Error> {
        switch(Array.find(owners,func(id : Principal) : Bool {id == caller})){
           case null return #err(#Invalid_Caller);
           case (?c) {
             switch(proposes.get(index)){
               case null return #err(#Invalid_Propose_index);
               case (?propose) {
                  switch(vote.get(index)){
                    case null {
                      vote.put(index, 1);
                      return #ok() 
                    };
                    case (?num){
                      vote.put(index, num + 1);
                      return #ok();
                    };
                  };
               };
             };
           };
        };            
    }; 


    public shared({caller}) func execPropose(index : Nat) : async Result.Result<(), Error> {
        switch(Array.find(owners,func(id : Principal) : Bool {id == caller})){
           case null return #err(#Invalid_Caller);
           case (?c) {
             switch(proposes.get(index)){
               case null return #err(#Invalid_Propose_index);
               case (?propose) {
                 switch(vote.get(index)) {
                   case null return #err(#Invalid_Propose_Result);
                   case (?num) {
                      if(num > M){
                        switch(propose.action){
                          case(#Install) {
                            let management : Management = actor("aaaaa-aa");
                            await management.install_code({ 
                                arg = [];
                                wasm_module = Option.unwrap(propose.wasm);
                                mode = #install;
                                canister_id = propose.principal;
                            });
                            return #ok();
                          };
                          case(#Add_Owner){
                            owners := Array.append<Principal>(owners,[propose.principal]);
                            return #ok();
                          };
                          case(#Del_Owner){return #ok();};
                          case(#Create_Canister){return #ok();};
                          case(#Start_Canister){return #ok();};
                          case(#Del_Canister){return #ok();};
                          case(#Stop_Canister){return #ok();};
                        };
                      } else {
                        return #err(#Invalid_Propose_Result);
                    };
                  };
                 };
               };
             };
           };
        };         
    };

    public shared({caller}) func putCanister(c : Canister) : async Result.Result<(), Error>{
        if(caller != owner){
            return #err(#Invalid_Caller)
        };
        canisters.put(c.canister_id, c);
        #ok(())
    };

    public type canister_id = Principal;

    public type wasm_module = [Nat8];

    public type canister_settings = {
        freezing_threshold : ?Nat;
        controllers : ?[Principal];
        memory_allocation : ?Nat;
        compute_allocation : ?Nat;
    };

    public type Management = actor {

        delete_canister : shared { canister_id : canister_id } -> async ();

        deposit_cycles : shared { canister_id : canister_id } -> async ();
        start_canister : shared { canister_id : canister_id } -> async ();
        stop_canister : shared { canister_id : canister_id } -> async ();
        install_code : shared {
            arg : [Nat8];
            wasm_module : wasm_module;
            mode : { #reinstall; #upgrade; #install };
            canister_id : canister_id;
            } -> async ();

        create_canister : shared { settings : ?canister_settings } -> async {
            canister_id : canister_id;
        };
        update_settings : ({
            canister_id : Principal;
            settings : canister_settings
        }) -> async ();

    };

    public type Memo = Nat64;

    public type Token = {
        e8s : Nat64;
    };

    public type TimeStamp = {
        timestamp_nanos: Nat64;
    };

    public type AccountIdentifier = Blob;

    public type SubAccount = Blob;

    public type BlockIndex = Nat64;

    public type TransferError = {
        #BadFee: {
            expected_fee: Token;
        };
        #InsufficientFunds: {
            balance: Token;
        };
        #TxTooOld: {
            allowed_window_nanos: Nat64;
        };
        #TxCreatedInFuture;
        #TxDuplicate : {
            duplicate_of: BlockIndex;
        };
    };

    public type TransferArgs = {
        memo: Memo;
        amount: Token;
        fee: Token;
        from_subaccount: ?SubAccount;
        to: AccountIdentifier;
        created_at_time: ?TimeStamp;
    };

    public type TransferResult = {
        #Ok: BlockIndex;
        #Err: TransferError;
    };

    public type Address = Blob;

    public type AccountBalanceArgs = {
        account : Address
    };

    type NotifyCanisterArgs = {
        // The of the block to send a notification about.
        block_height: BlockIndex;
        // Max fee, should be 10000 e8s.
        max_fee: Token;
        // Subaccount the payment came from.
        from_subaccount: ?SubAccount;
        // Canister that received the payment.
        to_canister: Principal;
        // Subaccount that received the payment.
        to_subaccount:  ?SubAccount;
    };

    type Ledger = actor{
        transfer : TransferArgs -> async TransferResult;
        account_balance : query AccountBalanceArgs -> async Token;
        notify_dfx : NotifyCanisterArgs -> async ();
    };

    let CYCLE_MINTING_CANISTER = Principal.fromText("rkp4c-7iaaa-aaaaa-aaaca-cai");
    let ledger : Ledger = actor("ryjl3-tyaaa-aaaaa-aaaba-cai");
    let TOP_UP_CANISTER_MEMO = 0x50555054 : Nat64;

    type DeployArgs = {
        name : Text;
        description : Text;
        settings : ?canister_settings;
        wasm : [Nat8];
        cycle_amount : Nat;
        preserve_wasm : Bool;
    };

    public shared({caller}) func deployCanister(
        args : DeployArgs
    ) : async Result.Result<Principal, Error>{
        if(caller != owner){
            return #err(#Invalid_Caller)
        };
        if(args.cycle_amount >= Cycles.balance()){
            return #err(#Insufficient_Cycles)
        };
        Cycles.add(args.cycle_amount);
        let management : Management = actor("aaaaa-aa");
        let _canister_id = (await management.create_canister({ settings = args.settings })).canister_id;
        canisters.put(_canister_id, {
            name = args.name;
            description = args.description;
            canister_id = _canister_id;
            wasm = if(args.preserve_wasm){
                ?args.wasm
            }else{
                null
            };
        });
        ignore await management.update_settings({
            canister_id = _canister_id;
            settings = {
                freezing_threshold = null;
                controllers = ?[Principal.fromActor(this), caller];
                memory_allocation = null;
                compute_allocation = null;
            }
        });
        ignore await management.install_code({
            arg = [];
            wasm_module = args.wasm;
            mode = #install;
            canister_id = _canister_id;
        });
        let record = {
            canister_id = _canister_id;
            method = #deploy;
            amount = args.cycle_amount;
            times = Time.now();
        };
        switch(records.get(record.canister_id)){
            case(null) {records.put(record.canister_id,[record])};
            case(?r){
                let p = Array.append(r,[record]);
                records.put(record.canister_id,p);
            }
        };
        #ok(_canister_id)
    };

    public shared({caller}) func startCanister(principal : Principal) : async Result.Result<Text, Text> {
        let management : Management = actor("aaaaa-aa");
        await management.start_canister({ canister_id = principal});
        let record = {
            canister_id = principal;
            method = #start;
            amount = 0;
            times = Time.now();
        };
        switch(records.get(record.canister_id)){
            case(null) {records.put(record.canister_id,[record])};
            case(?r){
                let p = Array.append(r,[record]);
                records.put(record.canister_id,p);
            }
        };
        #ok("start canister successfully")
        
    };

    public shared({caller}) func stopCanister(principal : Principal) : async Result.Result<Text, Text> {
        let management : Management = actor("aaaaa-aa");
        await management.stop_canister({ canister_id = principal});
        let record = {
            canister_id = principal;
            method = #stop;
            amount = 0;
            times = Time.now();
        };
        switch(records.get(record.canister_id)){
            case(null) {records.put(record.canister_id,[record])};
            case(?r){
                let p = Array.append(r,[record]);
                records.put(record.canister_id,p);
            }
        };
        #ok("stop canister successfully")

    };

    public shared({caller}) func depositCycles(
        id : Principal,
        cycle_amount : Nat,
    ) : async Result.Result<(), Error>{
        /// 0.01 T cycles 剩下
        if(cycle_amount + 10_000_000_000 >= Cycles.balance()){
            return #err(#Insufficient_Cycles)
        }else if(caller != owner){
            return #err(#Invalid_Caller)
        };
        let management : Management = actor("aaaaa-aa");
        Cycles.add(cycle_amount);
        ignore await management.deposit_cycles({ canister_id = id });
        let record = {
            canister_id = id;
            method = #deposit;
            amount = cycle_amount;
            times = Time.now();
        };
        switch(records.get(record.canister_id)){
            case(null) {records.put(record.canister_id,[record])};
            case(?r){
                let p = Array.append(r,[record]);
                records.put(record.canister_id,p);
            }
        };
        #ok(())
    };

    type InterfaceError = {
        #Insufficient_Cycles;
    };

    type CycleInterface = actor{
        withdraw_cycles : (to : ?Principal) -> async ();
    };

    public shared({caller}) func delCanister(
        id : Principal,
        cycle_to : ?Principal
    ) : async Result.Result<(), Error>{
        if(caller != owner){
            return #err(#Invalid_Caller)
        };
        // install wasm
        let management : Management = actor("aaaaa-aa");
        ignore await management.install_code({
            arg = [];
            wasm_module = cycle_wasm;
            mode = #reinstall;
            canister_id = id;
        });
        // call to interface
        let from : CycleInterface = actor(Principal.toText(id));
        await from.withdraw_cycles(cycle_to);
        ignore await management.stop_canister({canister_id = id });
        ignore await management.delete_canister({ canister_id = id });
        canisters.delete(id);
        #ok(())
    };

    public func wallet_receive() : async (){
        ignore Cycles.accept(Cycles.available())
    };

    system func preupgrade(){
        canisters_entries := Iter.toArray(canisters.entries());
        record_entries := Iter.toArray(records.entries());
    };

    system func postupgrade(){
        canisters_entries := [];
        record_entries := [];
    };
};