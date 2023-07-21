unit uNBody;

interface
  type

{    TPosition = class( TObject )
    private
      fx,
      fy,
      fz,
      fr,
      ftheta,
      fphi     : double;
    public
      
    end; // TPosition
}
    TBody = class( TCollectionItem ) // right now this will look more like a record
    private
      fm   : double;
      fx   : double;
      fy   : double;
      fVx  : double;
      fVy  : double;
      fFx  : double;
      fFy  : double;
    public
      constructor Create( Collection : TCollection;
                          x, y, Vx, Vy : double ); override;
    published
      property m  : double read fm write fm;
      property x  : double read fx write fx;
      property y  : double read fy write fy;
      property Vx : double read fVx write fVx;
      property Vy : double read fVy write fVy;
      property Fx : double read fFx write fFx;
      property Fy : double read fFy write fFy; 
    end; // TParticle

    TBodies = class( TCollection )
    private
      fNBodySystem : TNBodySystem; // holds owner of collection
      function  GetBody( i : integer ) : TBody;
      procedure SetBody( i : integer; Value : TBody );
    protected
      function GetOwner : TPersistent; override;
      procedure Update( Item: TCollectionItem ); override;
    public
      function Add : TBody;
      constructor Create( AOwner : TNBodySystem );
    published
      property Items[ index : integer ] : TBody read GetBody write SetBody;
    end; // TBodies

implementation

end.
 