type feature = int
type dataval = feature * int
type data = dataval array
type 'a trainval = {
	data : data ;
	category : 'a
}
type 'a trainset = {
	set : 'a trainval list ;
	nbFeatures : int ;
	featureMax : int array (* Max value for the feature a *)
}

module DVMap = Map.Make (struct type t = dataval let compare = compare end)

type 'a decisiontree = DecisionLeaf of 'a
	| DecisionNode of feature * 'a decisiontree DVMap.t
